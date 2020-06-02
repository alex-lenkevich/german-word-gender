package link.german.gender.trainer

import java.time
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}
import java.util.UUID

import io.circe.generic.AutoDerivation
import io.circe.{Decoder, Encoder}
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer2.enums.AnswerType.AnswerType
import link.german.gender.trainer2.enums.QuestionType.QuestionType
import link.german.gender.trainer2.enums.MemoryState.MemoryState
import link.german.gender.trainer2.enums._
import zio.ZIO

import scala.collection.Seq
import scala.math.Ordering
import scala.util.Random

object model extends AutoDerivation {

  implicit val answerTypeDecoder: Decoder[AnswerType] = Decoder.enumDecoder(AnswerType)
  implicit val answerTypeEncoder: Encoder[AnswerType] = Encoder.enumEncoder(AnswerType)
  implicit val questionTypeDecoder: Decoder[QuestionType] = Decoder.enumDecoder(QuestionType)
  implicit val questionTypeEncoder: Encoder[QuestionType] = Encoder.enumEncoder(QuestionType)

  implicit val FullListToList: FullWordList => WordList = _.states =>> WordList.apply

  trait WordListLike {
    val states: Seq[WordState]

    implicit val questionType: QuestionType = QuestionType.TR

    def predictPlan: String = Stream.iterate(LocalDateTime.now())(_.plusDays(1))
      .takeWhile(_.isBefore(LocalDateTime.now.plusDays(7)))
      .foldLeft((states, Map[LocalDateTime, Int]())) {
        case ((st, res), date) => ((
          st.map {
            case x if x.nextAskDate.exists(_.timeHasCome(date)) =>
              x.copy(answers = x.answers :+ Answer(AnswerType.Correct, QuestionType.TR, Seq(x.word.name), 5000, date))
            case x => x
          },
          res + (date -> st.count(_.nextAskDate.exists(_.timeHasCome(date)))))
          )
      }._2.toSeq.sorted.map(x => s"${x._1.toLocalDate}: ${x._2}").mkString("\n")

    def time: String = states.flatMap(_.answers).map(_.date)
      .groupBy(_.toLocalDate)
      .mapValues(ans =>
        ans.sorted.foldLeft[(Long, Seq[Long], Option[LocalDateTime])]((0L, Seq(0L), None)) {
          case ((time, sessions, None), date) => (time, sessions, Some(date))
          case ((time, sessions :+ last, Some(lastDate)), date) =>
            val sec = ChronoUnit.SECONDS.between(lastDate, date)
            if(sec < 60)
              ((time + sec), sessions :+ (last + sec), Some(date))
            else
              (time + 10, sessions :+ (last + 10) :+ 0L, Some(date))
        }
      )
      .toSeq
      .sortBy(_._1)
      .map {case (date, (dur, sessions, _)) => s"$date: ${dur.seconds.pretty}"} //  (${sessions.map(_.seconds.pretty).mkString(", ")})
      .mkString("\n")

    override def toString: String = Seq(
      states.mkString("==== Words =====\n", "\n" + "-"*106 + "\n", ""),
      states.groupBy(s => s.state match {
        case MemoryState.New => "1. New"
        case MemoryState.Learning => "2. Learning"
        case MemoryState.Temp => "3. Temp"
        case MemoryState.Hard => s"4. Hard (${s.effectiveCombo})"
        case MemoryState.InMemory if !s.comboDuration.minus(15.days).isNegative => "8. InMemory 15"
        case MemoryState.InMemory if !s.comboDuration.minus(7.days).isNegative => "7. InMemory 7"
        case MemoryState.InMemory if !s.comboDuration.minus(3.days).isNegative => "6. InMemory 3"
        case MemoryState.InMemory if !s.comboDuration.minus(1.days).isNegative => "5. InMemory 1"
        case MemoryState.Persistent => "9. Persistent"
      }).mapValues(_.length)
        .toSeq.sorted
        .map { case (s, c) => s"$s: $c" }
        .mkString("==== By State ====\n", "\n", ""),
      states.groupBy(state => state.nextAskDate.map { askDate =>
        val duration = ChronoUnit.DAYS.between(LocalDate.now(), askDate.toLocalDate)
        if (askDate.timeHasCome())
          1d -> "Ready to repeat"
        else if (askDate.timeHasCome(LocalDateTime.now().plusHours(1)))
          1.1 -> "In 1 hour"
        else if (askDate.timeHasCome(LocalDateTime.now().plusHours(4)))
          1.2 -> "In 4 hours"
        else if (duration == 0)
          2d -> "Today"
        else if (duration == 1)
          3d -> "Tomorrow"
        else if(duration <= 7)
          (duration.toInt + 2d) -> s"In ${duration} days"
        else 10d -> s"Later"
      }.getOrElse(0d -> "New")
      ).mapValues(_.length)
        .toSeq.sorted
        .map { case ((_, s), c) => s"$s: $c" }
        .mkString("==== By Ask Time ====\n", "\n", "")
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

    def active: WordList = this
      .filter(x => x.word.name.startsWith("ab"))
      .filter(!_.word.name.contains("geeignet"))
      .filter(!_.word.name.contains("ehrlich"))
      .filter(!_.word.name.contains("geehrt"))


    def answersLine: String = {
      active.states
        .map(x => s"${x.word.name.padTo(30, ' ')} ${x.state.toString.padTo(10, ' ')}: ${x.answersLine.getOrElse("NEW")}").mkString("\n")
    }

    def diff(daysAgo: Int = 0): String = {
      val cutoff = LocalDateTime.now().startOfDay.minusDays(daysAgo)
      val diffs = states.map(s =>
        s.copy(answers = s.answers.filter(_.date.isBefore(cutoff))) ->
        s.copy(answers = s.answers.filter(_.date.isBefore(cutoff.plusDays(1))))
      )
        .filter {
          case (o, n) => o.state != n.state
        }
      val total = diffs.groupBy { case (o, n) => o.state -> n.state }.mapValues(_.length)
        .toSeq.sorted
        .map {case ((o, n), c) => s"$o => $n: $c"}.mkString("\n")
      val words = diffs.sortBy{ case (o, n) => o.state -> n.state }
        .map {case (o, n) => s"${o.word.name}: ${o.state} => ${n.state}"}
        .mkString("\n")
      Seq(words, total).mkString("\n=========\n", "\n===========\n", "\n=============\n")
    }

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
    p3: Option[String] = None,
    pr: Option[String] = None,
    pp: Option[String] = None,
    examples: Option[Seq[(String, String)]] = None
  )

  object Word {
    val prefexRegex = "^(\\w+ )?(be|ge|ent|emp|er|ver|miß|zer|los|an|ab|bei|mit|ein|vor|nach|uber|über|zu|unter|um|aus|auf)+"
  }

  case class WordState(word: Word, id: String = UUID.randomUUID().toString, answers:  Seq[Answer] = Seq()) {

    import link.german.gender.trainer2.enums.MemoryState._

    implicit val questionType: QuestionType = QuestionType.TR


    def questionTypes: Seq[QuestionType] =
      Some(QuestionType.PP).filter(_ => word.pp.isDefined).toSeq ++
      Some(QuestionType.P3).filter(_ => word.p3.isDefined).toSeq ++
      Some(QuestionType.PR).filter(_ => word.pr.isDefined) :+
        QuestionType.TR

    def orderedAnswers(implicit questionType: QuestionType): Seq[Answer] =
      answers.filter(_.questionType == questionType).sortBy(_.date)

    def latestOpt(implicit questionType: QuestionType): Option[Answer] = orderedAnswers.sortBy(_.date).lastOption
    def latest(implicit questionType: QuestionType): Answer = latestOpt.get
    def latestSuccessOpt(implicit questionType: QuestionType): Option[Answer] = combo.headOption
    def combo(implicit questionType: QuestionType): Seq[Answer] = orderedAnswers.reverse.takeWhile(_.isCorrect)
    def effectiveCombo(implicit questionType: QuestionType): Int = combo.sortBy(_.date).foldLeft(Seq[Answer]()){
      case (Seq(), a) => Seq(a)
      case (seq :+ tail, a) if (a.date - tail.date).minus(1.hours).isNegative =>
        seq :+ tail
      case (seq :+ tail, a) => seq :+ tail :+ a
    }.length
    def comboDates(implicit questionType: QuestionType): Seq[LocalDateTime] = combo.map(_.date)
    def comboDuration(implicit questionType: QuestionType): time.Duration = (for {a <- comboDates.minOpt; b <- comboDates.maxOpt} yield b - a).getOrElse(java.time.Duration.ZERO)
    def allCorrect(implicit questionType: QuestionType): Boolean = orderedAnswers.forall(_.isCorrect)
    def length(implicit questionType: QuestionType): Int = orderedAnswers.length

    def state(implicit questionType: QuestionType): MemoryState = {
      if (orderedAnswers.isEmpty) New
      else if (allCorrect && orderedAnswers.forall(_.attempts.length <= 1)) Persistent
      else if (comboDuration.toDays >= 14 && combo.length >= 5) Persistent
      else if (orderedAnswers.length > 5 && effectiveCombo < 5 && comboDuration.toDays < 1) Hard
      else if (combo.length >= 2 && comboDuration.toDays < 1) Temp
      else if (combo.length >= 2) InMemory
      else Learning
    }

    def nextAskDate(implicit questionType: QuestionType): Option[LocalDateTime] = (state match {
      case Persistent => Some(latest.date + 30.days)
      case InMemory | Temp => Some(latest.date.toLocalDate
        .plusDays(comboDuration.toDays * 2 + 1)
        .atStartOfDay()
        .plusHours(5))
      case Hard => Some {
        latest.date.plus(
          if(!latest.isCorrect) 10.seconds
          else if(comboDuration.isZero) 1.minutes
          else if(comboDuration.minus(1.hours).isNegative) 15.minutes
          else 1.hours
        )
      }
      case _ => latestOpt.map(_.date.plusSeconds(5))
    }).map(_.past2min())

    def shouldBeAsked(implicit questionType: QuestionType): Boolean =
      nextAskDate.forall(_.timeHasCome())

    def withAnswer(answer: Answer): WordState = this.copy(answers = answers :+ answer)

    override def equals(obj: Any): Boolean = obj match {
      case that: WordState => id == that.id
      case _ => false
    }

    def answersLine(implicit questionType: QuestionType): Option[String] = {
      for {
        min <- orderedAnswers.map(_.date.toLocalDate).minOpt
        max <- (orderedAnswers.map(_.date.toLocalDate) ++ nextAskDate.map(_.toLocalDate)).maxOpt
      } yield {
        Stream.iterate(min)(_.plusDays(1)).takeWhile(!_.isAfter(max))
          .map(d => d -> orderedAnswers.filter(_.date.toLocalDate == d).minByOpt(_.date)).map {
          case (_, Some(a)) => if(a.isCorrect) 'O' else 'x'
          case (d, None) if d == max => 'A'
          case _ => '_'
        }.mkString
      }
    }

    override def canEqual(that: Any): Boolean = {
      equals(that)
    }

    def order(implicit questionType: QuestionType): String = state match {
      case Learning => "0"
      case Temp => "1"
      case InMemory => "2"
      case Hard => "3" + effectiveCombo.toString.padTo(4, ' ') + comboDuration.getSeconds.toString.padTo(20, ' ')
      case New => "4" + word.name.toLowerCase.replaceAll(Word.prefexRegex, "")
      case Persistent => "5"
    }

    override def toString: String = s"${word.name.trim.grouped(20).map(_.padTo(20, ' ')).mkString(" |\n")} | ${word.translate.replaceAll("[^0-9a-zA-Zа-яА-Я)(\\[\\]. ]", "").trim.grouped(40).map(_.padTo(40, ' ')).mkString(s" |\n${" "*21}| ")} | ${state.toString.padTo(10, ' ')} " +
      s" | ${nextAskDate.fold("--")(_.pretty).padTo(4, ' ')}" +
      s" | $effectiveCombo (${comboDuration.prettyShort.padTo(3, ' ')})" +
      s" | -${latestSuccessOpt.fold("--")(t => (LocalDateTime.now() - t.date).prettyShort)}"

    override def hashCode(): Int = id.hashCode
  }

  case class Answer(`type`: AnswerType, questionType: QuestionType, attempts: Seq[String] = Seq(), time: Long, date: LocalDateTime = now) {
    def isCorrect: Boolean = `type` == AnswerType.Correct && attempts.length <= 4
  }

  object ExitCommand extends Exception
  case class EditCommand(f: WordList => TZIO[WordList]) extends Exception

  case class ExitException(state: WordList) extends Exception


}
