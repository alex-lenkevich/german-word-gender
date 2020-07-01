package link.german.gender.trainer2

import java.time
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}

import link.german.gender.MongoDbClient.WordGender
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.model._
import link.german.gender.trainer2.test.TestType.{KasusTestType, Translate}
import link.german.gender.trainer2.test._
import zio.ZIO

import scala.collection.Seq
import scala.math.BigDecimal.RoundingMode
import scala.math.Ordering
import scala.util.Random

object model {

  import link.german.gender.trainer2.enums.MemoryState._

  implicit val FullListToList: FullWordList => WordList = _.states =>> WordList.apply

  type TZIO[T] = ZIO[zio.ZEnv, Throwable, T]

  trait WordListLike {
    val states: Seq[WordTestResults]

    def predictPlan: String = Stream.iterate(LocalDateTime.now())(_.plusDays(1))
      .takeWhile(_.isBefore(LocalDateTime.now.plusDays(7)))
      .foldLeft((states, Map[LocalDateTime, Int]())) {
        case ((st, res), date) => ((
          st.map {
            case x if x.nextAsk.exists(_._2.timeHasCome(date)) =>
              x.withAnswer(Answer(correct = true, TestMethod.Text, Seq(x.testType.answer(x.data)), 0, date))
            case x => x
          },
          res + (date -> st.count(_.nextAsk.exists(_._2.timeHasCome(date)))))
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
        case New => "1. New"
        case Learning => "2. Learning"
        case Temp => "3. Temp"
        case Hard => s"4. Hard (${s.effectiveCombo})"
        case InMemory if !s.comboDuration.minus(15.days).isNegative => "8. InMemory 15"
        case InMemory if !s.comboDuration.minus(7.days).isNegative => "7. InMemory 7"
        case InMemory if !s.comboDuration.minus(3.days).isNegative => "6. InMemory 3"
        case InMemory if !s.comboDuration.minus(1.days).isNegative => "5. InMemory 1"
        case Persistent => "9. Persistent"
      }).mapValues(_.length)
        .toSeq.sorted
        .map { case (s, c) => s"$s: $c" }
        .mkString("==== By State ====\n", "\n", ""),
      states.groupBy(state => state.nextAsk.map { case (_, askDate) =>
        val duration = ChronoUnit.DAYS.between(LocalDate.now(), askDate.toLocalDate)
        if (askDate.timeHasCome())
          1d -> "Ready to repeat"
        else if (askDate.timeHasCome(LocalDateTime.now().plusMinutes(15)))
          1.1 -> "In 15 mins"
        else if (askDate.timeHasCome(LocalDateTime.now().plusHours(1)))
          1.2 -> "In 1 hour"
        else if (askDate.timeHasCome(LocalDateTime.now().plusHours(4)))
          1.3 -> "In 4 hours"
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

  case class FullWordList(states: Seq[WordTestResults]) extends WordListLike {
    def :+(word: WordTestResults): FullWordList = FullWordList(states.filterNot(word.==) :+ word)

    def ++(words: WordListLike): FullWordList = FullWordList(states.diff(words.states) ++ words.states)
  }

  case class WordList(states: Seq[WordTestResults]) extends WordListLike {
    def :+(state: WordTestResults): WordList = WordList(states.filterNot(state.==) :+ state)

    def ++(words: WordListLike): WordList = WordList(states.diff(words.states) ++ words.states)

    def filter(f: WordTestResults => Boolean): WordList = WordList(states.filter(f))

    def sorted(implicit ord: Ordering[String]): WordList = WordList(states.sortBy(_.order))

    def sortBy[B](f: WordTestResults => B)(implicit ord: Ordering[B]): WordList = WordList(states.sortBy(f))

    def take(n: Int): WordList = WordList(states.take(n))

    def shuffle: WordList = WordList(Random.shuffle(states))

    def partition(f: WordTestResults => Boolean): (WordList, WordList) = {
      val (a, b) = states.partition(f)
      WordList(a) -> WordList(b)
    }

    def map(f: WordTestResults => WordTestResults): WordList = WordList(states.map(f))

    def collect(f: PartialFunction[WordTestResults, WordTestResults]): WordList = WordList(states.collect(f))

    def active: WordList = this
//      .filter(x => !x.data.de.matches(".*n( \\(sich\\))?$"))
      .filter(x => x.testType == Translate)
      .filter(!_.data.de.contains("geeignet"))
      .filter(!_.data.de.contains("ehrlich"))
      .filter(!_.data.de.contains("geehrt"))

    def answersLine: String = {
      active.states
        .map(x => s"${x.data.de.padTo(30, ' ')} ${x.state.toString.padTo(10, ' ')}: ${x.answersLine.getOrElse("NEW")}").mkString("\n")
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
        .map {case (o, n) => s"${o.data.de}: ${o.state} => ${n.state}"}
        .mkString("\n")
      Seq(words, total).mkString("\n=========\n", "\n===========\n", "\n=============\n")
    }

  }

  case class WordTestResults(data: WordData, testType: TestType, answers: Seq[Answer]) {

    def sortedAnswers: Seq[Answer] = answers.sortBy(_.date)

    def withAnswer(answer: Answer): WordTestResults = copy(
      answers = answers :+ answer
    )

    def latestOpt: Option[Answer] = sortedAnswers.sortBy(_.date).lastOption

    def latest: Answer = latestOpt.get

    def latestSuccessOpt: Option[Answer] = combo.headOption

    def combo: Seq[Answer] = sortedAnswers.reverse.takeWhile(_.isCorrect)

    def effectiveCombo: Int = combo.sortBy(_.date).foldLeft(Seq[Answer]()) {
      case (Seq(), a) => Seq(a)
      case (seq :+ tail, a) if (a.date - tail.date).minus(1.hours).isNegative =>
        seq :+ tail
      case (seq :+ tail, a) => seq :+ tail :+ a
    }.length

    def comboDates: Seq[LocalDateTime] = combo.map(_.date)

    def comboDuration: time.Duration = (for {a <- comboDates.minOpt; b <- comboDates.maxOpt} yield b - a).getOrElse(java.time.Duration.ZERO)

    def allCorrect: Boolean = sortedAnswers.forall(_.isCorrect)

    def length: Int = answers.length

    def state: MemoryState = {
      if (answers.isEmpty) New
      else if (allCorrect && answers.forall(_.attempts.length <= 1)) Persistent
      else if (comboDuration.toDays >= 14 && combo.length >= 5) Persistent
      else if (answers.length > 5 && effectiveCombo < 5 && comboDuration.toHours < 2) Hard
      else if (combo.length >= 2 && comboDuration.toDays < 1) Temp
      else if (combo.length >= 2) InMemory
      else Learning
    }

    def nextAskDate: Option[LocalDateTime] = (state match {
      case Persistent => Some(latest.date.toLocalDate
        .plusDays(45)
        .atStartOfDay()
        .plusHours(5))
      case InMemory | Temp => Some(latest.date.toLocalDate
        .plusDays(comboDuration.toDays * 2 + 1)
        .atStartOfDay()
        .plusHours(5))
      case Hard => Some {
        latest.date.plus(
          if (!latest.isCorrect) 10.seconds
          else if (comboDuration.isZero) 1.minutes
          else if (comboDuration.minus(1.hours).isNegative) 15.minutes
          else 1.hours
        )
      }
      case _ => latestOpt.map(_.date.plusSeconds(5))
    }).map(_.past2min())

    def nextAsk: Option[(TestMethod, LocalDateTime)] = nextAskDate.map(nextAskMethod -> _)

    def nextAskMethod: TestMethod = testType match {
      case TestType.Translate if state == Hard && !latest.isCorrect => TestMethod.Select
      case TestType.Translate => TestMethod.Text
      case _ => TestMethod.Select
    }

    def shouldBeAsked: Boolean =
      nextAsk.forall(_._2.timeHasCome())


    def order: String = state match {
      case Learning => "0"
      case Temp => "1"
      case InMemory => "2"
      case Hard => "3" + hardness.toString.padTo(6, ' ') + effectiveCombo.toString.padTo(4, ' ') + comboDuration.getSeconds.toString.padTo(20, ' ')
      case New => "4" + testTypeOrder + wordOrder
      case Persistent => "5"
    }

    def testTypeOrder: String = testType match {
      case Translate => "0"
      case KasusTestType => "1"
      case _ => "2"
    }

    def wordOrder: String = if(data.de.matches("^[A-ZÄÜÖ]")) {
      "0"
    } else if(data.de.endsWith("n") || data.perfekt.isDefined) {
      "2"
    } else {
      "1"
    } + data.de.toLowerCase.replaceAll(Word.prefexRegex, "")


    def question: String = testType.question(this.data)

    def answer: String = testType.answer(this.data)


    def answersLine: Option[String] = {
      for {
        min <- sortedAnswers.map(_.date.toLocalDate).minOpt
        max <- (sortedAnswers.map(_.date.toLocalDate) ++ nextAskDate.map(_.toLocalDate)).maxOpt
      } yield {
        Stream.iterate(min)(_.plusDays(1)).takeWhile(!_.isAfter(max))
          .map(d => d -> sortedAnswers.filter(_.date.toLocalDate == d).minByOpt(_.date)).map {
          case (_, Some(a)) => if(a.isCorrect) 'O' else 'x'
          case (d, None) if d == max => 'A'
          case _ => '_'
        }.mkString
      }
    }

    lazy val answersSessions: Seq[(LocalDateTime, Boolean)] = sortedAnswers.foldLeft(Seq[(LocalDateTime, Boolean)]()){
      case (seq :+ ((date, correct)), answer) if (answer.date - date).abs().minus(1.hours).isNegative =>
        seq :+ date -> (correct && answer.correct)
      case (seq, answer) =>
        seq :+ answer.date -> answer.isCorrect
    }

    lazy val hardness: Option[BigDecimal] = if(answers.isEmpty) {
      None
    } else {
      Some((BigDecimal(answersSessions.count(_._2)) / answersSessions.length).setScale(2, RoundingMode.HALF_UP))
    }

    override def equals(obj: Any): Boolean = obj match {
      case that: WordTestResults => data.id == that.data.id && testType == that.testType
      case _ => false
    }
    override def hashCode(): Int = (data.id -> testType).hashCode()

    override def canEqual(that: Any): Boolean = {
      equals(that)
    }

    override def toString: String = {
      s"${answer.trim.grouped(20).map(_.padTo(20, ' ')).mkString(" |\n")} | ${question.replaceAll("[^0-9a-zA-Zа-яА-Я)(\\[\\]. ]", "").trim.grouped(40).map(_.padTo(40, ' ')).mkString(s" |\n${" "*21}| ")} | ${state.toString.padTo(10, ' ')} " +
        s" | ${nextAskDate.fold("--")(_.pretty).padTo(4, ' ')}" +
        s" | $effectiveCombo (${comboDuration.prettyShort.padTo(3, ' ')})" +
        s" | -${latestSuccessOpt.fold("--")(t => (LocalDateTime.now() - t.date).prettyShort)}" +
        s" | ${hardness.getOrElse(" -- ")}"
    }
  }

  case class WordData(
    id: String,
    de: String,
    ru: String,
    `type`: Option[WordType] = None,
    kasus: Option[Kasus] = None,
    plural: Option[String] = None,
    genetiv: Option[String] = None,
    present3: Option[String] = None,
    prateritum: Option[String] = None, perfekt: Option[String] = None) {

    override def equals(obj: Any): Boolean = obj match {
      case that: WordState => id == that.id
      case _ => false
    }
    override def hashCode(): Int = id.hashCode


    override def canEqual(that: Any): Boolean = {
      equals(that)
    }
  }


  sealed trait Kasus {
    def article: String
  }

  sealed trait WordType

  object WordType {
    case object Noun extends WordType
    case object Verb extends WordType
    case object Adj extends WordType
    case object Adv extends WordType
  }


  object Kasus {
    case object M extends Kasus {
      def article: String = "der"
      override def toString: String = "m"
    }
    case object F extends Kasus {
      def article: String = "die"
      override def toString: String = "f"
    }
    case object N extends Kasus {
      def article: String = "das"
      override def toString: String = "n"
    }
    case object P extends Kasus {
      def article: String = "die"
      override def toString: String = "p"
    }
  }

  case class Answer(correct: Boolean, method: TestMethod, attempts: Seq[String] = Seq(), time: Long, date: LocalDateTime = now) {
    def isCorrect: Boolean = method.isCorrect(this)
  }

  case class ExitException(state: WordList) extends Exception

}
