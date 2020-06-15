package link.german.gender.trainer2

import java.io.{File, FileInputStream}
import java.nio.file.Files

import javazoom.jl.player.Player
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums._
import link.german.gender.trainer2.model._
import link.german.gender.trainer2.test._
import link.german.gender.trainer2.ui.{SelectTest, TextTest, Window}
import link.german.gender.{LingvoClient, PonsAudioDownloader}
import zio._
import zio.console.{putStrLn, _}

import scala.collection.Seq
import scala.util.Random
import scala.util.matching.Regex

object Main extends App with PonsAudioDownloader with StateService with LingvoClient {

  import MemoryState._


  val WordFilterRegex: Regex = """^_(.+)$""".r
  val DiffRegex: Regex = """^d(iff)?(\d*)$""".r
  val LimitRegex: Regex = """^(\d+)$""".r

  def execute(command: String)(list: WordList, prevCommands: Seq[String]): ZIO[zio.ZEnv, Throwable, (WordList, Seq[String])] = command match {
    case "" => UIO(list -> (prevCommands :+ "l 20 -s"))
    case "+k" => Task(list.filter(_.testType == TestType.KasusTestType) -> prevCommands)
    case "+p3" => Task(list.filter(_.testType == TestType.Present3) -> prevCommands)
    case "+pp" => Task(list.filter(_.testType == TestType.Perfekt) -> prevCommands)
    case "+pr" => Task(list.filter(_.testType == TestType.Prateritum) -> prevCommands)
    case "+t" => Task(list.filter(_.testType == TestType.Translate) -> prevCommands)
    case "-a" => Task(list.filter(_.nextAsk.exists(_._2.timeHasCome())) -> prevCommands)
    case "-h" => Task(list.filter(_.state == Hard) -> prevCommands)
    case "-l" => Task(list.filter(_.state == Learning) -> prevCommands)
    case "-m" => Task(list.filter(_.state == InMemory) -> prevCommands)
    case "-n" => Task(list.filter(_.state == New) -> prevCommands)
    case "-p" => Task(list.filter(_.state == Persistent) -> prevCommands)
    case "-p3" => Task(list.filter(w => w.testType == TestType.Present3) -> prevCommands)
    case "-pp" => Task(list.filter(w => w.testType == TestType.Perfekt) -> prevCommands)
    case "-pr" => Task(list.filter(w => w.testType == TestType.Prateritum) -> prevCommands)
    case "-s" => Task(list.filter(_.shouldBeAsked) -> prevCommands)
    case "-t" => Task(list.filter(_.state == Temp) -> prevCommands)
    case "a" => UIO(list -> prevCommands) <* putStrLn(list.answersLine)
    case "i" => UIO(list -> prevCommands) <* putStrLn(list.toString())
    case "l" => (list.filter(_.shouldBeAsked) =>> runTest).map(_ -> (prevCommands :+ "i -a"))
    case "l!" => UIO(list -> (prevCommands ++ Seq("l 20 -a", "l! -a").filter(_ => list.states.nonEmpty)))
    case "p" => UIO(list -> prevCommands) <* putStrLn(list.predictPlan)
    case "t" => UIO(list -> prevCommands) <* putStrLn(list.time)
    case DiffRegex(_, days) => UIO(list -> prevCommands) <* putStrLn(list.diff(if(days == "") 0 else days.toInt))
    case LimitRegex(n) => Task(list.take(n.toInt) -> prevCommands)
    case WordFilterRegex(word) => UIO(list.filter(s => word.r.findFirstMatchIn(s.data.de.toLowerCase).isDefined) -> prevCommands)
    case x => UIO(list -> prevCommands) <* putStrLn(s"Unknown command $x")
  }

  def runTest(_list: WordList): TZIO[WordList] = {
    Window.visible = true
    def ask(l: WordList): ZIO[Console, Throwable, WordList] = {
      Random.shuffle(l.states.filter(_.shouldBeAsked)) match {
        case Seq() => ZIO.effectTotal(l)
        case word +: tail =>
          for {
//            _ <- putStrLn(l.states.mkString("\n=== Current session ===\n", "\n", "\n=======\n"))
            wordAnswer <- Task {
              val testUi = word.nextAskMethod match {
                case TestMethod.Select =>
                  new SelectTest(word, word.testType.options(word.data, _list.states.map(_.data), 6),
                    (l.states.length - tail.length - 1) -> l.states.length)
                case TestMethod.Text =>
                  new TextTest(word, (l.states.length - tail.length - 1) -> l.states.length)
              }
              Window.contents = testUi.panel
              Window.visible = true
              testUi.getAnswer
            } <* Task{
              val player = new Player(new FileInputStream(getPonsAudio(word.answer)))
              player.play()
              player.close()
            }.fork
            _ <- putStrLn(wordAnswer.toString)
            newList = l :+ word.withAnswer(wordAnswer)
            result <- newList =>> ask
          } yield result
      }
    }
    ask(_list) <* Task(Window.dispose())
  }

  def makeSession(list: FullWordList, commands: Seq[String]): TZIO[Int] = (for {
    _ <- putStrLn("learn | learn hard | learn active | info | info hard | info session | info <word>")
    command <- commands.headOption.fold(getStrLn)(UIO(_))
    (updatedWords, newCommands) <- command.split(" ")
      .foldRight[ZIO[zio.ZEnv, Throwable, (WordList, Seq[String])]](
        UIO(list.active.shuffle.sortBy(_.order) -> Seq())
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
        Files.copy(new File("trainer_state_2.json").toPath, {
          val file = new File(s"backup/trainer_state_2_${System.currentTimeMillis()}.json")
          file.toPath
        })
        Files.copy(new File("trainer_words_2.json").toPath, {
          val file = new File(s"backup/trainer_words_2_${System.currentTimeMillis()}.json")
          file.toPath
        })
      }
      words <- loadWords()
      initState <- loadState(words)
      _ <- makeSession(initState, Seq("i"))
    } yield 0
  }.catchAll{x => UIO{x.printStackTrace()} *> putStrLn(x.getMessage) *> UIO(1)}
}


