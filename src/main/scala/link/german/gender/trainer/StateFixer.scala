package link.german.gender.trainer

import link.german.gender.trainer2
import link.german.gender.LingvoClient
import link.german.gender.trainer2.enums.AnswerType.AnswerType
import link.german.gender.trainer2.enums.{AnswerType, QuestionType}
import link.german.gender.trainer.model.{Answer, Word, WordList, WordState}
import link.german.gender.trainer2.model.{Kasus, WordData}
import link.german.gender.trainer2.test.{TestMethod, TestType}
import zio._

import scala.io.Source
import scala.util.Try

object StateFixer extends App with StateService with LingvoClient {

  private val service2 = new Object with trainer2.StateService

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      state <- loadState
      newState <- Task {
        trainer2.model.FullWordList(
          state.states.flatMap { r =>
            val word = WordData(r.id, r.word.name, r.word.translate, r.word.genus.map(_.head match {
              case 'm' => Kasus.M
              case 'f' => Kasus.F
              case 'n' => Kasus.N
              case 'p' => Kasus.P
            }), r.word.p3, r.word.pr, r.word.pp)
            Seq(
              r.word.pr.map(_ => trainer2.model.WordTestResults(word, TestType.Prateritum,
                r.answers.filter(_.questionType == QuestionType.PR).map(convertAnswer)
              )),
              r.word.pr.map(_ => trainer2.model.WordTestResults(word, TestType.Perfekt,
                r.answers.filter(_.questionType == QuestionType.PP).map(convertAnswer)
              )),
              r.word.pr.map(_ => trainer2.model.WordTestResults(word, TestType.Present3,
                r.answers.filter(_.questionType == QuestionType.P3).map(convertAnswer)
              ))
            ).flatten :+ trainer2.model.WordTestResults(word, TestType.Translate,
              r.answers.filter(_.questionType == QuestionType.TR).map(convertAnswer)
            )
          }
        )
      }
      _ <- service2.saveState(newState)
      _ <- service2.saveWords(newState.states.map(_.data))
    } yield 0
    }.orDie

  private def convertAnswer(a: Answer) = {
    trainer2.model.Answer(
      a.`type` == AnswerType.Correct, TestMethod.Text, attempts = a.attempts, a.time, a.date
    )
  }
}
