package link.german.gender.trainer

import java.io.FileInputStream
import java.util.concurrent.Executors

import javazoom.jl.player.Player
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.Main.getPonsAudio
import link.german.gender.trainer2.enums.AnswerType.{AnswerType, Correct, Incorrect}
import link.german.gender.trainer2.enums.QuestionType
import link.german.gender.trainer2.enums.QuestionType.QuestionType
import link.german.gender.trainer.model.{Answer, ExitCommand, TZIO, Word, WordList, WordState}
import org.apache.commons.text.similarity.{LevenshteinDetailedDistance, LevenshteinDistance}
import zio.console.{putStrLn, _}
import zio.{Schedule, UIO, ZIO}

import scala.collection.Seq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.util.Random

trait LearningService {

  implicit val questionType: QuestionType = QuestionType.TR

  private val executorService = Executors.newWorkStealingPool(1)
  implicit val soundEc: ExecutionContextExecutor = ExecutionContext.fromExecutor(executorService)

  sys.addShutdownHook(
    () => executorService.shutdown()
  )

  def textExamples(list: WordList): TZIO[WordList] = {
    list =>> textInputTest { x =>
      val (de, ru) = Random.shuffle(x.examples.get).head
      val name = """\{\{([^}]+)\}\}""".r.findFirstMatchIn(de).get.group(1).trim
      val question = ru + "\n" + de.replaceAll("""\{\{.+\}\}""", "_______")
      name -> question
    }
  }

  def p3Forms(list: WordList): TZIO[WordList] = {
    textInputTest{ x => x.p3.get -> x.name }(QuestionType.P3)(list)
  }

  def ppForms(list: WordList): TZIO[WordList] = {
    textInputTest{ x => x.pp.get -> x.name }(QuestionType.PP)(list)
  }

  def textInputTest(fun: Word => (String, String) = x => x.name -> x.translate)(implicit questionType: QuestionType = QuestionType.TR):
              WordList => TZIO[WordList] = { list =>

    def checkAnswer(word: Word, time: Long, answer: String): TZIO[Either[String, AnswerType]] = {
      val correct = fun(word)._1
      val results = LevenshteinDetailedDistance.getDefaultInstance.apply(answer, correct)
      if (answer == "?") {
        for {
          _ <- playSound(word) &>
            ( putStrLn(s"# $correct") *>
              getStrLn *>
              putStr("\033[H\033[2J") *>
              readAnswer
            ).repeat(Schedule.doUntil(_._1 == correct))
        } yield Right(Incorrect)
      } else if (results.getDistance == 0) {
        playSound(word) &> ZIO.effectTotal(Right(Correct))
      } else if (answer == correct.replaceAll("(die|das|der) ", "")) {
        UIO(Left("# Mit dem Artikel!"))
      } else {
        UIO(Left(s"# Nein (-${results.getDeleteCount} +${results.getInsertCount} ≠${results.getSubstituteCount})!"))
//        UIO(Left(s"# Nein (D: ${results.getDistance})!"))
      }
    }

    def getAnswerText(word: Word, attempts: Seq[String] = Seq()): TZIO[Answer] = for {
      (answer, time) <- readAnswer.repeat(Schedule.doWhile(_._1 == ""))
      (answerOrMessage) <- checkAnswer(word, time, answer)
      answer <- answerOrMessage.fold({ message =>
        putStrLn(message) *> getAnswerText(word, attempts :+ answer)
      }, answerType => UIO(Answer(answerType, questionType, attempts :+ answer, time)))
    } yield answer

    def testWord(word: WordState): TZIO[Answer] = {
      for {
        _ <- putStrLn(s"# ${fun(word.word)._2}")
        answer <- getAnswerText(word.word)
      } yield answer
    }

    Random.shuffle(list.states.filter(_.shouldBeAsked(questionType))) match {
      case Seq() => ZIO.effectTotal(list)
      case word +: tail =>
        for {
          _ <- putStrLn(list.states.mkString("\n=== Current session ===\n", "\n", "\n=======\n"))
          _ <- putStr("\033[H\033[2J")
          completedPercent = 50 * (list.states.length - tail.length - 1) / list.states.length
          _ <- putStrLn("Progress: [" + ("#" * completedPercent) + ("_" * (50 - completedPercent)) + "]")
          wordAnswer <- testWord(word)
          newList = list :+ word.withAnswer(wordAnswer)
          result <- newList =>> textInputTest(fun)
        } yield result
    }
  }

  def hardWordsTest(list: WordList): TZIO[WordList] = for {
    learnedBySelection <- selectWordsTest(list, Map())
    learnedByTextInput <- learnedBySelection =>> textInputTest()
  } yield learnedByTextInput

  def selectWordsTest(list: WordList, selectResults: Map[WordState, Seq[Long]]): TZIO[WordList] = {
    Random.shuffle(
      list.states
        .filter(_.shouldBeAsked)
        .filterNot(selectResults.getOrElse(_, Seq()).count(_ < 10000) >= 2)
    ) match {
      case Seq() => UIO(list)
      case word +: _ =>
        for {
          _ <- putStr("\033[H\033[2J")
          _ <- putStrLn(s"## ${word.word.translate}")
          options = (list.states.filterNot(word == _)
            .sortBy(x => LevenshteinDistance.getDefaultInstance.apply(x.word.name, word.word.name))
            .take(5) :+ word
            ).map(_.word.name).sorted.zipWithIndex.map(x => x._2.toString -> x._1).toMap
          _ <- putStr(options.toSeq.sorted.map(opt => s"${opt._1}) ${opt._2}").mkString("======\n", "\n", "\n>> "))
          startTime = System.currentTimeMillis()
          input <- getStrLn.map(x => options.getOrElse(x, x))
          res <- if (input == word.word.name) {
            val newSelectResults = selectResults + (word -> (selectResults.getOrElse(word, Seq()) :+ (System.currentTimeMillis() - startTime)))
            (playSound(word.word) &> putStrLn("Correct")) *>
              selectWordsTest(list, newSelectResults)
          } else {
            (playSound(word.word) &> putStrLn(s"Incorrect: ${word.word.name}")) *>
              selectWordsTest(list, selectResults)
          }
        } yield res
    }
  }

  def readAnswer: TZIO[(String, Long)] = for {
    _ <- putStr("> ")
    start = System.currentTimeMillis()
    answer <- getStrLn.map(_.trim)
    r <- if (answer == "exit") {
      ZIO.fail(ExitCommand)
    } else {
      ZIO.effectTotal(answer -> (System.currentTimeMillis() - start))
    }
  } yield r


  def playSound(word: Word): TZIO[Unit] = ZIO {
    soundEc.execute { () =>
      val filePath = getPonsAudio(word.name)
      val player = new Player(new FileInputStream(filePath))
      player.play()
      player.close()
    }
  }.tapError { e =>
    e.printStackTrace()
    throw e
  }

}
