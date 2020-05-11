package link.german.gender.trainer

import java.time
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import io.circe.generic.AutoDerivation
import io.circe.{Decoder, Encoder}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.enums.AnswerType.AnswerType
import link.german.gender.trainer.enums._
import zio.ZIO

import scala.collection.Seq
import scala.math.Ordering
import scala.util.Random

object model extends AutoDerivation {

  implicit val genderDecoder: Decoder[AnswerType] = Decoder.enumDecoder(AnswerType)
  implicit val genderEncoder: Encoder[AnswerType] = Encoder.enumEncoder(AnswerType)

  implicit val FullListToList: FullWordList => WordList = _.states =>> WordList.apply

  trait WordListLike {
    val states: Seq[WordState]

    def predictPlan: String = Stream.iterate(LocalDateTime.now())(_.plusDays(1))
      .takeWhile(_.isBefore(LocalDateTime.now.plusMonths(1)))
      .foldLeft((states, Map[LocalDateTime, Int]())) {
      case ((st, res), date) => ((
        st.map {
          case x if x.nextAskDate.exists(_.timeHasCome(date)) =>
            x.copy(answers = x.answers :+ Answer(AnswerType.Correct, Seq(x.word.name), 5000, date))
          case x => x
        },
          res + (date -> st.count(_.nextAskDate.exists(_.timeHasCome(date)))))
        )
    }._2.toSeq.sorted.map(x => s"${x._1.toLocalDate}: ${x._2}").mkString("\n")

    override def toString: String = Seq(
      states.mkString("==== Words =====\n", "\n", ""),
      states.groupBy(_.state match {
        case MemoryState.New => "1. New"
        case MemoryState.Learning => "2. Learning"
        case MemoryState.Hard => "3. Hard"
        case MemoryState.InMemory => "4. InMemory"
        case MemoryState.Persistent => "5. Persistent"
      }).mapValues(_.length)
        .toSeq.sorted
        .map { case (s, c) => s"$s: $c" }
        .mkString("==== By Ask Time ====\n", "\n", ""),
        states.groupBy(state => state.nextAskDate.map { askDate =>
        val duration = askDate - LocalDateTime.now()
        if(askDate.timeHasCome())
          1 -> "Ready to repeat"
        else if(askDate.toLocalDate == LocalDate.now)
          2 -> "Today"
        else if(askDate.toLocalDate == LocalDate.now.plusDays(1))
          3 -> "Tomorrow"
        else
          (duration.toDays.toInt + 2) -> s"In ${duration.toDays} days"
      }.getOrElse(0 -> "New")
      ).mapValues(_.length)
        .toSeq.sorted
        .map { case ((_, s), c) => s"$s: $c" }
        .mkString("==== By state ====\n", "\n", "")
    ).mkString("\n")
  }

  case class FullWordList(states: Seq[WordState]) extends WordListLike {
    def :+(word: WordState): FullWordList = FullWordList(states.filterNot(word.==) :+ word)

    def ++(words: WordListLike): FullWordList = FullWordList(states.diff(words.states) ++ words.states)
  }

  case class WordList(states: Seq[WordState]) extends WordListLike {
    def :+(word: WordState): WordList = WordList(states.filterNot(word.==) :+ word)

    def ++(words: WordListLike): WordList = WordList(states.diff(words.states) ++ words.states)

    def filter(f: WordState => Boolean): WordList = WordList(states.filter(f))

    def sorted(implicit ord: Ordering[String]): WordList = WordList(states.sortBy(_.order))

    def sortBy[B](f: WordState => B)(implicit ord: Ordering[B]): WordList = WordList(states.sortBy(f))

    def take(n: Int): WordList = WordList(states.take(n))

    def shuffle: WordList = WordList(Random.shuffle(states))

    def partition(f: WordState => Boolean): (WordList, WordList) = {
      val (a, b) = states.partition(f)
      WordList(a) -> WordList(b)
    }
    def map(f: WordState => WordState): WordList = WordList(states.map(f))
    def collect(f: PartialFunction[WordState, WordState]): WordList = WordList(states.collect(f))

    def active: WordList = this.filter(_.word.name.matches("^(die|der) .*"))
  }

  type TZIO[T] = ZIO[zio.ZEnv, Throwable, T]

  case class Word(
    name: String,
    translate: String,
    wordClass: String,
    flexion: Option[String] = None,
    genus: Option[String] = None,
    verbClass: Option[String] = None,
    info: Option[String] = None,
    examples: Option[Seq[(String, String)]] = None
  )

  object Word {
    val prefexRegex = "^(\\w+ )?(be|ge|ent|emp|er|ver|miß|zer|los|an|ab|bei|mit|ein|vor|nach|uber|über|zu|unter|um|aus|auf)+"
  }

  case class WordState(word: Word, id: String = UUID.randomUUID().toString, answers: Seq[Answer] = Seq()) {

    import link.german.gender.trainer.enums.MemoryState._

    lazy val orderedAnswers: Seq[Answer] = answers.sortBy(_.date)

    lazy val latestOpt: Option[Answer] = orderedAnswers.sortBy(_.date).lastOption
    lazy val latest: Answer = latestOpt.get
    lazy val latestSuccessOpt: Option[Answer] = combo.headOption
    lazy val combo: Seq[Answer] = orderedAnswers.reverse.takeWhile(_.isCorrect)
    lazy val comboDates: Seq[LocalDateTime] = combo.map(_.date)
    lazy val comboDuration: time.Duration = (for {a <- comboDates.minOpt; b <- comboDates.maxOpt} yield b - a).getOrElse(java.time.Duration.ZERO)
    lazy val allCorrect: Boolean = orderedAnswers.forall(_.isCorrect)
    lazy val length: Int = orderedAnswers.length

    lazy val state: MemoryState = {
      if (orderedAnswers.isEmpty) New
      else if (allCorrect && orderedAnswers.forall(_.attempts.length <= 1)) Persistent
      else if (comboDuration.toDays >= 14 && combo.length >= 5) Persistent
      else if (orderedAnswers.length > 5 && comboDuration.toMinutes <= 15) Hard
      else if (combo.length >= 2) InMemory
      else Learning
    }

    def nextAskDate: Option[LocalDateTime] = (state match {
      case Persistent => Some(latest.date + 30.days)
      case InMemory => Some(latest.date.toLocalDate
        .plusDays(comboDuration.toDays * 2 + 1)
        .atStartOfDay()
        .plusHours(5))
      case Hard => Some {
        if (latest.isCorrect) {
          latest.date + (comboDuration.toMinutes * 5 + 1).minutes
        } else {
          latest.date + 10.seconds
        }
      }
      case _ => latestOpt.map(_.date.plusSeconds(5))
    }).map(_.past2min())

    def shouldBeAsked: Boolean =
      nextAskDate.forall(_.timeHasCome())

    def latestNCorrect(n: Int): Boolean = answers.reverse.take(n).forall(_.isCorrect)

    def withAnswer(answer: Answer): WordState = this.copy(answers = answers :+ answer)

    override def equals(obj: Any): Boolean = obj match {
      case that: WordState => id == that.id
      case _ => false
    }

    override def canEqual(that: Any): Boolean = {
      equals(that)
    }

    def order: String = state match {
      case Hard => "0"
      case InMemory => "1"
      case Learning => "2"
      case New => "3" + word.name.toLowerCase.replaceAll(Word.prefexRegex, "")
      case Persistent => "4"
    }

    override def toString: String = s"${word.name.padTo(20, ' ')}| ${word.translate.replaceAll("[^0-9a-zA-Zа-яА-Я)(\\[\\]. ]", "").trim.padTo(40, ' ')} => ${state.toString.padTo(20, ' ')}" +
      s"next ask: ${nextAskDate.fold("--")(_.pretty).padTo(10, ' ')}, combo: ${combo.length} (${comboDuration.pretty})"

    override def hashCode(): Int = id.hashCode
  }

  case class Answer(`type`: AnswerType, attempts: Seq[String] = Seq(), time: Long, date: LocalDateTime = now) {
    def isCorrect: Boolean = `type` == AnswerType.Correct && attempts.length <= 3
  }

  object ExitCommand extends Exception

  case class ExitException(state: WordList) extends Exception


}
