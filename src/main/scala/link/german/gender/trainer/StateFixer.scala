package link.german.gender.trainer

import link.german.gender.client.DudenClient
import link.german.gender.trainer.model.Answer
import link.german.gender.trainer2.enums.AnswerType
import link.german.gender.trainer2.test.TestMethod
import link.german.gender.{LingvoClient, trainer2}
import zio._
import zio.console._

import java.io.File
import java.nio.file.Files
import scala.io.Source

object StateFixer extends App with StateService with LingvoClient with DudenClient {

  private val service2 = new Object with trainer2.StateService

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    ZIO.effectTotal(Source.fromFile("verbs.tsv")).bracket(x => UIO(x.close()), { verbs =>
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
  //      words <- service2.loadWords("backup/trainer_words_2_1591341408055.json")
        words <- service2.loadWords()
        _ <- putStrLn(words.filter(x => x.de.matches("^[A-ZÄÜÖ].*") && x.kasus.isEmpty).mkString("\n"))
//        _ <- service2.saveWords((words ++ verbs.getLines().grouped(3)
//          .filterNot(x => words.exists(_.de == x.head))
//            .map {case Seq(de, ru, _) => WordData(
//              id = UUID.randomUUID().toString,
//              de = de,
//              ru = ru
//            )}
//          ).groupBy(_.id).mapValues(x => x.find(_.plural.isDefined).getOrElse(x.head)).values.toSeq)
      } yield 0
    }).orDie

  private def convertAnswer(a: Answer) = {
    trainer2.model.Answer(
      a.`type` == AnswerType.Correct, TestMethod.Text, attempts = a.attempts, a.time, a.date
    )
  }
}
