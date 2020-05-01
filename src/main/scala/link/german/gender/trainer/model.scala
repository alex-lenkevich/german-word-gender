package link.german.gender.trainer

import java.time
import java.time.LocalDateTime
import java.util.UUID

import io.circe.generic.AutoDerivation
import io.circe.{Decoder, Encoder}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.enums.AnswerType
import link.german.gender.trainer.enums.AnswerType.AnswerType

import scala.concurrent.duration.Duration

object model extends AutoDerivation {

  implicit val genderDecoder: Decoder[AnswerType] = Decoder.enumDecoder(AnswerType)
  implicit val genderEncoder: Encoder[AnswerType] = Encoder.enumEncoder(AnswerType)


  case class Word(
    name: String,
    translate: String,
    wordClass: String,
    flexion: Option[String] = None,
    genus: Option[String] = None,
    verbClass: Option[String] = None,
    info: Option[String] = None
  )

  case class WordState(word: Word, id: String = UUID.randomUUID().toString, answers: Seq[Answer] = Seq()) {
    import link.german.gender.trainer.enums.MemoryState._

    lazy val orderedAnswers: Seq[Answer] = answers.sortBy(_.date)

    lazy val latestOpt: Option[Answer] = orderedAnswers.sortBy(_.date).lastOption
    lazy val latest: Answer = latestOpt.get
    lazy val combo: Seq[Answer] = orderedAnswers.reverse.takeWhile(_.isCorrect)
    lazy val comboDates: Seq[LocalDateTime] = combo.map(_.date)
    lazy val comboDuration: time.Duration = (for{a <- comboDates.minOpt; b <- comboDates.maxOpt} yield b-a).getOrElse(java.time.Duration.ZERO)
    lazy val allCorrect: Boolean = orderedAnswers.forall(_.isCorrect)
    lazy val length: Int = orderedAnswers.length

    lazy val state: MemoryState = {
      if(orderedAnswers.isEmpty) New
      else if(allCorrect && orderedAnswers.forall(_.attempts == 1)) Persistent
      else if(comboDuration.toDays >= 14 && combo.length >= 5) Persistent
      else if(orderedAnswers.length > 5 && comboDuration.toMinutes <= 15) Hard
      else if(combo.length >= 2) InMemory
      else Learning
    }

    def nextAskDate: Option[LocalDateTime] = (state match {
      case Persistent => Some(latest.date + 30)
      case InMemory => Some(latest.date + comboDuration.toDays + 1)
      case _ => latestOpt.map(_.date.plusSeconds(3))
    }).map(_.past2min)

    def shouldBeAsked: Boolean = nextAskDate.forall(_.timeHasCome)

    def latestNCorrect(n: Int): Boolean = answers.reverse.take(n).forall(_.isCorrect)

    def withAnswer(answer: Answer): WordState = this.copy(answers = answers :+ answer)

    override def equals(obj: Any): Boolean = obj match {
      case that: WordState => id == that.id
      case _ => false
    }

    override def canEqual(that: Any): Boolean = {
      equals(that)
    }

    override def toString: String = s"${word.name.padTo(20, ' ')} => ${state.toString.padTo(20, ' ')} " +
      s"next ask: ${nextAskDate.fold("--")(_.pretty).padTo(10, ' ')}, combo: ${combo.length} (${comboDuration} days)"

    override def hashCode(): Int = id.hashCode
  }

  case class Answer(`type`: AnswerType, attempts: Int, time: Long, date: LocalDateTime = now) {
    def isCorrect: Boolean = `type` == AnswerType.Correct && attempts <= 3
  }

}
