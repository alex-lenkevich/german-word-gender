package link.german.gender.trainer2

import java.io.FileWriter

import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import io.circe.parser._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Printer}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.model.{TZIO, WordState}
import link.german.gender.trainer2.model.{Answer, FullWordList, Kasus, WordData, WordTestResults, WordType}
import link.german.gender.trainer2.test.{TestMethod, TestType}
import zio.{Task, UIO, ZIO}

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
    case "K" => TestType.KasusTestType
  }

  implicit def TestTypeEncoder: Encoder[TestType] = (a: TestType) => (a match {
    case TestType.Translate => "T"
    case TestType.Present3 => "P3"
    case TestType.Prateritum => "PR"
    case TestType.Perfekt => "PP"
    case TestType.KasusTestType => "K"
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

  implicit def WordTypeDecoder: Decoder[WordType] = (c: HCursor) => c.as[String].map {
    case "N" => WordType.Noun
    case "V" => WordType.Verb
    case "Adj" => WordType.Adj
    case "Adv" => WordType.Adv
  }

  implicit def WordTypeEncoder: Encoder[WordType] = (a: WordType) => (a match {
    case WordType.Noun => "N"
    case WordType.Verb => "V"
    case WordType.Adj => "Adj"
    case WordType.Adv => "Adv"
  }).asJson

  implicit val AnswerDecoder: Decoder[Answer] = deriveDecoder[Answer]
  implicit val AnswerEncoder: Encoder[Answer] = deriveEncoder[Answer]
  implicit val StateDTODecoder: Decoder[StateDTO] = deriveDecoder[StateDTO]
  implicit val StateDTOEncoder: Encoder[StateDTO] = deriveEncoder[StateDTO]
  implicit val WordDataDecoder: Decoder[WordData] = deriveDecoder[WordData]
  implicit val WordDataEncoder: Encoder[WordData] = deriveEncoder[WordData]

  def loadWords(wordFile: String = "trainer_words_2.json"): Task[Seq[WordData]] = Task.fromEither {
    val sourceWords: BufferedSource = Source.fromFile(wordFile)
    val words = for {
      jsonWords <- sourceWords.getLines().mkString =>> parse
      words <- jsonWords.as[Seq[WordData]]
    } yield words
    sourceWords.close()
    words
  }

  def loadState(words: Seq[WordData], stateFile: String = "trainer_state_2.json"): Task[FullWordList] = {
    UIO.effectTotal(Source.fromFile(stateFile)).bracket(x => UIO(x.close()), { sourceState =>
       for {
        jsonState <- ZIO.fromEither(sourceState.getLines().mkString =>> parse)
        state <- ZIO.fromEither(jsonState.as[Seq[StateDTO]])
      } yield
        FullWordList(words.flatMap {word =>
          Seq(
            TestType.Translate,
            TestType.KasusTestType,
            TestType.Present3,
            TestType.Prateritum,
            TestType.Perfekt
          ).filter(_.isSupported(word)).map(
            WordTestResults(word, _, Seq())
          )
        }) ++ FullWordList(state.map(s => WordTestResults(words.find(_.id == s.wordId).get, s.testType, s.answers)))

    })
  }

  def saveState(state: FullWordList): TZIO[FullWordList] = Task {
    val json = state.states.map(s => StateDTO(s.data.id, s.testType, s.answers)).distinct.asJson.pretty(printer)
    val writer = new FileWriter("trainer_state_2.json")
    writer.write(json)
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
