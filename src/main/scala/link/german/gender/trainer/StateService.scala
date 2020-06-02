package link.german.gender.trainer

import java.io.FileWriter
import java.time.LocalDateTime

import io.circe.parser._
import io.circe.syntax._
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums.QuestionType.QuestionType
import link.german.gender.trainer2.enums.{MemoryState, QuestionType}
import link.german.gender.trainer.model.{WordState, _}
import link.german.gender.trainer2.model.{Answer, WordData}
import link.german.gender.trainer2.test.TestType
import zio.Task

import scala.collection.Seq
import scala.io.{BufferedSource, Source}

trait StateService {

  implicit val questionType: QuestionType = QuestionType.TR

  def loadState: Task[FullWordList] = Task.fromEither {
    val source: BufferedSource = Source.fromFile("trainer_state.json")
    val state = for {
      json <- source.getLines().mkString =>> parse
      state <- json.as[Seq[WordState]]
    } yield state
    source.close()
    val time = LocalDateTime.now.plusWeeks(1)
    state.map(_
      .filterNot(_.state == MemoryState.Persistent)
      .filter(_.nextAskDate.exists(_.isBefore(time)))
      .sortBy(_.nextAskDate))
    state.map(FullWordList)
  }

  def saveState(state: FullWordList): TZIO[FullWordList] = Task {
    val writer = new FileWriter("trainer_state.json")
    state.states.asJson.noSpaces =>> writer.write
    writer.close()
    state
  }


}
