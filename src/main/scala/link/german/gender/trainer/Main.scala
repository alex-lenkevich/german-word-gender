package link.german.gender.trainer

import java.io.File
import java.nio.file.Files

import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.enums.MemoryState
import link.german.gender.trainer.model._
import link.german.gender.{LingvoClient, PonsAudioDownloader}
import zio._
import zio.console.{putStrLn, _}

import scala.util.matching.Regex

object Main extends App with PonsAudioDownloader with StateService with LearningService with LingvoClient {

  val infoFilterRegex: Regex = """info (.+)""".r

  def makeSession(list: FullWordList, commands: Seq[String]): TZIO[Int] = (for {
    _ <- putStrLn("learn | learn hard | learn active | info | info hard | info session | info <word>")
    command <- commands.headOption.fold(getStrLn)(UIO(_))
    (updatedWords, newCommands) <- command match {
      case "info" =>
        UIO(list -> Seq()) <* putStrLn(list.active.sortBy(_.word.name).toString())
      case "info active" =>
        UIO(list -> Seq()) <* printActive(list)
      case "info hard" =>
        UIO(list -> Seq()) <* putStrLn(list.active.filter(_.state == MemoryState.Hard).sortBy(_.word.name).toString())
      case "info session" =>
        UIO(list -> Seq()) <* putStrLn(selectWordsFromNextSession(list.active).toString())
      case infoFilterRegex(word) =>
        UIO(list -> Seq()) <* putStrLn(list.active.filter(_.word.name.contains(word)).sortBy(_.word.name).toString())
      case "learn hard" =>
        makeSelectSession(list.active).map(_ -> Seq())
      case "plan" => UIO(list -> Seq()) <* putStrLn(list.active.predictPlan)
      case "learn" => makeTextSession(list.active).map(_ -> Seq("info active"))
      case "learn active" => (selectActiveWords(list) =>> textInputTest()).map(_ -> Seq("info active"))
      case "examples" =>
        (selectWordsFromNextSession(list) =>> withExamples =>> textExamples).map(_ -> Seq("learn examples"))
      case "learn examples" =>
        (selectWordsFromNextSession(list.filter(_.word.examples.exists(_.nonEmpty))) =>>
          withExamples =>>
          textExamples).map(_ -> Seq("info active"))
      case "" =>
        UIO(list -> Seq("learn"))
      case _ => UIO(list -> Seq())
    }
    newList = list ++ updatedWords
    _ <- if(list != newList) saveState(newList) else ZIO.unit
    _ <- makeSession(newList, commands.drop(1) ++ newCommands)
    n <- ZIO.never
  } yield n).catchSome {
    case ExitException(s) => saveState(list ++ s) *> UIO(0)
  }

  private def printActive(list: FullWordList) = {
    putStrLn(selectActiveWords(list).toString())
  }

  private def selectActiveWords(list: FullWordList) = {
    list.active.filter(x => x.state != MemoryState.New && x.shouldBeAsked).sortBy(_.word.name)
  }

  private def withExamples(list: WordList) = {
    list.map{ws =>
      if(ws.word.examples.isEmpty) {
        val word = readFromLingvo(ws.word.name)
        ws.copy( word = ws.word.copy(translate = word.translate, examples = word.examples) )
      } else ws
    }
  }

  private def makeSelectSession(list: WordList) = {
    hardWordsTest(list.filter(_.state == MemoryState.Hard))
  }

  private def makeTextSession(list: WordList) = {
    selectWordsFromNextSession(list) =>> textInputTest()
  }

  private def selectWordsFromNextSession(list: WordList) = {
    list.active.filter(_.shouldBeAsked).shuffle.sorted.take(20)
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
      _ <- makeSession(initState, Seq("info"))
    } yield 0
  }.catchAll(x => putStrLn(x.getMessage) *> UIO(1))
}


