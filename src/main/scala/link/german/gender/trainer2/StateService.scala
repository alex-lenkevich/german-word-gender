package link.german.gender.trainer2

import java.io.FileWriter

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Printer}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.model.TZIO
import link.german.gender.trainer2.model.{Answer, FullWordList, Kasus, WordData, WordTestResults}
import link.german.gender.trainer2.test.{TestMethod, TestType}
import zio.Task

import scala.collection.Seq
import scala.io.{BufferedSource, Source}

trait StateService {

  implicit val circeConfiguration: Configuration = Configuration.default
    .withSnakeCaseMemberNames.withDiscriminator("type")

  val printer: Printer = Printer.spaces2.copy(dropNullValues = true)

  implicit def TestTypeDecoder: Decoder[TestType] = (c: HCursor) => c.as[String].map {
    case "T" => TestType.Translate
    case "P3" => TestType.Present3
    case "PR" => TestType.Prateritum
    case "PP" => TestType.Perfekt
  }

  implicit def TestTypeEncoder: Encoder[TestType] = (a: TestType) => (a match {
    case TestType.Translate => "T"
    case TestType.Present3 => "P3"
    case TestType.Prateritum => "PR"
    case TestType.Perfekt => "PP"
  }).asJson

  implicit def TestMethodDecoder: Decoder[TestMethod] = (c: HCursor) => c.as[String].map {
    case "T" => TestMethod.Text
    case "S" => TestMethod.Select
  }
  implicit def TestMethodEncoder: Encoder[TestMethod] = (a: TestMethod) => (a match {
    case TestMethod.Text => "T"
    case TestMethod.Select => "S"
  }).asJson

  implicit def KasusDecoder: Decoder[Kasus] = (c: HCursor) => c.as[String].map {
    case "F" => Kasus.F
    case "M" => Kasus.M
    case "N" => Kasus.N
    case "P" => Kasus.P
  }

  implicit def KasusEncoder: Encoder[Kasus] = (a: Kasus) => (a match {
    case Kasus.F => "F"
    case Kasus.M => "M"
    case Kasus.N => "N"
    case Kasus.P => "P"
  }).asJson

  implicit val AnswerDecoder: Decoder[Answer] = deriveDecoder[Answer]
  implicit val AnswerEncoder: Encoder[Answer] = deriveEncoder[Answer]
  implicit val StateDTODecoder: Decoder[StateDTO] = deriveDecoder[StateDTO]
  implicit val StateDTOEncoder: Encoder[StateDTO] = deriveEncoder[StateDTO]
  implicit val WordDataDecoder: Decoder[WordData] = deriveDecoder[WordData]
  implicit val WordDataEncoder: Encoder[WordData] = deriveEncoder[WordData]

  def loadState: Task[FullWordList] = Task.fromEither {
    val sourceWords: BufferedSource = Source.fromFile("trainer_words_2.json")
    val sourceState: BufferedSource = Source.fromFile("trainer_state_2.json")
    val state = for {
      jsonState <- sourceState.getLines().mkString =>> parse
      jsonWords <- sourceWords.getLines().mkString =>> parse
      state <- jsonState.as[Seq[StateDTO]]
      words <- jsonWords.as[Seq[WordData]]
    } yield FullWordList(state.map(s => WordTestResults(words.find(_.id == s.wordId).get, s.testType, s.answers)))
    sourceWords.close()
    sourceState.close()
    state
  }

  def saveState(state: FullWordList): TZIO[FullWordList] = Task {
    val writer = new FileWriter("trainer_state_2.json")
    state.states.map(s => StateDTO(s.data.id, s.testType, s.answers)).distinct.asJson.pretty(printer) =>> writer.write
    writer.close()
    state
  }

  def saveWords(state: Seq[WordData]): TZIO[Unit] = Task {
    val writer = new FileWriter("trainer_words_2.json")
    state.asJson.pretty(printer) =>> writer.write
    writer.close()
  }

  case class StateDTO(wordId: String, testType: TestType, answers: Seq[Answer])

}
