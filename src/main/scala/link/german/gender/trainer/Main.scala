package link.german.gender.trainer

import java.io.File
import java.nio.file.Files

import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums.{MemoryState, QuestionType}
import link.german.gender.trainer.model._
import link.german.gender.{LingvoClient, PonsAudioDownloader}
import zio._
import zio.console.{putStrLn, _}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums.QuestionType.QuestionType

import scala.io.Source
import scala.util.matching.Regex

object Main extends App with PonsAudioDownloader with StateService with LearningService with LingvoClient {

  val WordFilterRegex: Regex = """^\+(.+)$""".r
  val DiffRegex: Regex = """^d(iff)?(\d*)$""".r
  val LimitRegex: Regex = """^(\d+)$""".r

  override implicit val questionType: QuestionType = QuestionType.TR


  def execute(command: String)(list: WordList, prevCommands: Seq[String]): ZIO[zio.ZEnv, Throwable, (WordList, Seq[String])] = command match {
    case "new" | "-n" => Task(list.filter(_.state == MemoryState.New) -> prevCommands)
    case "learning" | "-l" => Task(list.filter(_.state == MemoryState.Learning) -> prevCommands)
    case "inmemory" | "-m" => Task(list.filter(_.state == MemoryState.InMemory) -> prevCommands)
    case "hard" | "-h" => Task(list.filter(_.state == MemoryState.Hard) -> prevCommands)
    case "persistent" | "-p" => Task(list.filter(_.state == MemoryState.Persistent) -> prevCommands)
    case "temp" | "-t" => Task(list.filter(_.state == MemoryState.Temp) -> prevCommands)
    case "active" | "-a" => Task(list.filter(_.nextAskDate.exists(_.timeHasCome())) -> prevCommands)
    case "session" | "-s" => Task(list.filter(_.shouldBeAsked) -> prevCommands)
    case "-pp" => Task(list.filter(_.word.pp.isDefined) -> prevCommands)
    case "-p3" => Task(list.filter(_.word.p3.isDefined) -> prevCommands)
    case "-pr" => Task(list.filter(_.word.pr.isDefined) -> prevCommands)
    case "info" | "i" => UIO(list -> prevCommands) <* putStrLn(list.toString())
    case DiffRegex(_, days) => UIO(list -> prevCommands) <* putStrLn(list.diff(if(days == "") 0 else days.toInt))
    case LimitRegex(n) => Task(list.take(n.toInt) -> prevCommands)
    case "answers" | "a" => UIO(list -> prevCommands) <* putStrLn(list.answersLine)
    case "plan" | "p" => UIO(list -> prevCommands) <* putStrLn(list.predictPlan)
    case "examples" | "e" => (list =>> withExamples =>> textExamples).map(_ -> prevCommands)
    case "text" | "l" => (list.filter(_.shouldBeAsked) =>> textInputTest()).map(_ -> (prevCommands :+ "info active"))
    case "select" | "s" => selectWordsTest(list.filter(_.shouldBeAsked), Map()).map(_ -> (prevCommands :+ "info active"))
    case "time" | "t" => UIO(list -> prevCommands) <* putStrLn(list.time)
    case "p3" => (list.filter(_.shouldBeAsked(QuestionType.P3)) =>> p3Forms).map(_ -> (prevCommands :+ "info active"))
    case "pp" => (list.filter(_.shouldBeAsked(QuestionType.PP)) =>> ppForms).map(_ -> (prevCommands :+ "info active"))
    case WordFilterRegex(word) => UIO(list.filter(s => word.r.findFirstMatchIn(s.word.name.toLowerCase).isDefined) -> prevCommands)
    case "" => UIO(list -> (prevCommands :+ "l 20 -s"))
    case x => UIO(list -> prevCommands) <* putStrLn(s"Unknown command $x")
  }

  def makeSession(list: FullWordList, commands: Seq[String]): TZIO[Int] = (for {
    _ <- putStrLn("learn | learn hard | learn active | info | info hard | info session | info <word>")
    command <- commands.headOption.fold(getStrLn)(UIO(_))
    (updatedWords, newCommands) <- command.split(" ")
      .foldRight[ZIO[zio.ZEnv, Throwable, (WordList, Seq[String])]](
        UIO(list.active.sortBy(_.order) -> Seq())
      ){
        case (cmd, prev) => prev.flatMap { case (l, cmds) => execute(cmd)(l, cmds) }
      }
    newList = list ++ updatedWords
    _ <- if(list != newList) saveState(newList) else ZIO.unit
    _ <- makeSession(newList, commands.drop(1) ++ newCommands)
    n <- ZIO.never
  } yield n).catchSome {
    case ExitException(s) => saveState(list ++ s) *> UIO(0)
  }

  private def withExamples(list: WordList) = {
    list.map{ws =>
      if(ws.word.examples.isEmpty) {
        val word = readFromLingvo(ws.word.name)
        ws//.copy( word = ws.word.copy(translate = word.translate, examples = word.examples) )
      } else ws
    }
  }

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      _ <- Task {
//        GlobalScreen.addNativeKeyListener(GlobalKeyListener)
//        GlobalScreen.setEventDispatcher(Executors.newSingleThreadExecutor)
//        GlobalScreen.registerNativeHook()
      }
      _ <- Task {
        Files.copy(new File("trainer_state.json").toPath, {
          val file = new File(s"backup/trainer_state_${System.currentTimeMillis()}.json")
          file.toPath
        })
      }
      initState <- loadState
      source = Source.fromFile("trainer_state_changes.txt")
      fixedState = source.getLines().map(_.split("=")).foldLeft(initState){
        case (state, Array(key, value)) => state ++ WordList(state.states.find(_.word.name == key).map(s => s.copy(word = s.word.copy(translate = value))).toSeq)
      }
      _ <- makeSession(fixedState, Seq("i"))
    } yield 0
  }.catchAll(x => putStrLn(x.getMessage) *> UIO(1))
}


