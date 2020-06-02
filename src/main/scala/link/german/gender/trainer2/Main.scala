package link.german.gender.trainer2

import java.io.File
import java.nio.file.Files

import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums._
import link.german.gender.trainer2.model._
import link.german.gender.trainer2.test._
import link.german.gender.trainer2.ui.{TextTest, Window}
import link.german.gender.{LingvoClient, PonsAudioDownloader}
import zio._
import zio.console.{putStrLn, _}

import scala.collection.Seq
import scala.util.Random
import scala.util.matching.Regex

object Main extends App with PonsAudioDownloader with StateService with LingvoClient {

  import MemoryState._


  val WordFilterRegex: Regex = """^\+(.+)$""".r
  val DiffRegex: Regex = """^d(iff)?(\d*)$""".r
  val LimitRegex: Regex = """^(\d+)$""".r

  def execute(command: String)(list: WordList, prevCommands: Seq[String]): ZIO[zio.ZEnv, Throwable, (WordList, Seq[String])] = command match {
    case "new" | "-n" => Task(list.filter(_.state == New) -> prevCommands)
    case "learning" | "-l" => Task(list.filter(_.state == Learning) -> prevCommands)
    case "inmemory" | "-m" => Task(list.filter(_.state == InMemory) -> prevCommands)
    case "hard" | "-h" => Task(list.filter(_.state == Hard) -> prevCommands)
    case "persistent" | "-p" => Task(list.filter(_.state == Persistent) -> prevCommands)
    case "temp" | "-t" => Task(list.filter(_.state == Temp) -> prevCommands)
    case "active" | "-a" => Task(list.filter(_.nextAsk.exists(_._2.timeHasCome())) -> prevCommands)
    case "session" | "-s" => Task(list.filter(_.shouldBeAsked) -> prevCommands)
    case "-pp" => Task(list.filter(w => w.testType == TestType.Perfekt) -> prevCommands)
    case "-p3" => Task(list.filter(w => w.testType == TestType.Present3) -> prevCommands)
    case "-pr" => Task(list.filter(w => w.testType == TestType.Prateritum) -> prevCommands)
    case "info" | "i" => UIO(list -> prevCommands) <* putStrLn(list.toString())
    case DiffRegex(_, days) => UIO(list -> prevCommands) <* putStrLn(list.diff(if(days == "") 0 else days.toInt))
    case LimitRegex(n) => Task(list.take(n.toInt) -> prevCommands)
    case "answers" | "a" => UIO(list -> prevCommands) <* putStrLn(list.answersLine)
    case "plan" | "p" => UIO(list -> prevCommands) <* putStrLn(list.predictPlan)
    case "learn" | "l" => (list.filter(_.shouldBeAsked) =>> runTest).map(_ -> (prevCommands :+ "info active"))
    case "time" | "t" => UIO(list -> prevCommands) <* putStrLn(list.time)
    case WordFilterRegex(word) => UIO(list.filter(s => word.r.findFirstMatchIn(s.data.de.toLowerCase).isDefined) -> prevCommands)
    case "" => UIO(list -> (prevCommands :+ "l 20 -s"))
    case x => UIO(list -> prevCommands) <* putStrLn(s"Unknown command $x")
  }

  def runTest(_list: WordList): TZIO[WordList] = {
    Window.visible = true
    def ask(l: WordList): ZIO[Console, Throwable, WordList] = {
      Random.shuffle(l.states.filter(_.shouldBeAsked)) match {
        case Seq() => ZIO.effectTotal(l)
        case word +: tail =>
          for {
            _ <- putStrLn(l.states.mkString("\n=== Current session ===\n", "\n", "\n=======\n"))
            completedPercent = 50 * (l.states.length - tail.length - 1) / l.states.length
            _ <- putStrLn("Progress: [" + ("#" * completedPercent) + ("_" * (50 - completedPercent)) + "]")
            wordAnswer <- Task {
              val testUi = new TextTest(word)
              Window.contents = testUi.panel
              Window.visible = true
              testUi.getAnswer
            }
            _ <- putStrLn(wordAnswer.toString)
            newList = l :+ word.withAnswer(wordAnswer)
            result <- newList =>> ask
          } yield result
      }
    }
    ask(_list) <* Task(Window.close())
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

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      _ <- Task {
        Files.copy(new File("trainer_state.json").toPath, {
          val file = new File(s"backup/trainer_state_${System.currentTimeMillis()}.json")
          file.toPath
        })
      }
      initState <- loadState
      _ <- makeSession(initState, Seq("i"))
    } yield 0
  }.catchAll(x => putStrLn(x.getMessage) *> UIO(1))
}


