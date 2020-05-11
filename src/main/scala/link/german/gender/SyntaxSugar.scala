package link.german.gender

import java.time.{Duration, LocalDate, LocalDateTime, OffsetDateTime, Period}
import java.time.temporal.ChronoUnit

import zio.ZIO

object SyntaxSugar {

  implicit val localDateOrdering: Ordering[LocalDate] = (x: LocalDate, y: LocalDate) => x.compareTo(y)
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  implicit class `collections sugar`[A](val date: TraversableOnce[A]) extends AnyVal {
    def maxOpt(implicit cmp: Ordering[A]): Option[A] = date.reduceOption(cmp.max)
    def minOpt(implicit cmp: Ordering[A]): Option[A] = date.reduceOption(cmp.min)
  }

  implicit class LocalDateTimeSyntaxSugar(date: LocalDateTime) {
    def +(duration: Duration): LocalDateTime = date.plus(duration)
    def -(duration: Duration): LocalDateTime = date.minus(duration)
    def -(anotherDate: LocalDateTime): Duration = Duration.between(anotherDate, date)
    def -(days: Number): LocalDateTime = date.minusDays(days.longValue())
    def timeHasCome(now: LocalDateTime = LocalDateTime.now): Boolean = !date.isAfter(now)
    def past2min(now: LocalDateTime = LocalDateTime.now): LocalDateTime = if(date.timeHasCome(now)) LocalDateTime.MIN else date

    def startOfDay: LocalDateTime = date.truncatedTo(ChronoUnit.DAYS)
    def max(another: LocalDateTime): LocalDateTime = if(another.isAfter(date)){
      another
    }  else {
      date
    }
    def min(another: LocalDateTime): LocalDateTime = if(another.isAfter(date)){
      date
    }  else {
      another
    }

    def pretty: String = if (date.isBefore(LocalDateTime.now)) {
      "Now"
    } else {
      val days = ChronoUnit.DAYS.between(LocalDateTime.now, date)
      val hours = ChronoUnit.HOURS.between(LocalDateTime.now, date)
      val minutes = ChronoUnit.MINUTES.between(LocalDateTime.now, date)
      if(days != 0) s"In $days days" else
      if(hours != 0) s"In $hours hours" else
      if(minutes != 0) s"In $minutes minutes" else "Now"
    }
  }

  implicit class LongDateSyntaxSugar(i: Long) {
    def days: Duration = Duration.ofDays(i.longValue())
    def minutes: Duration = Duration.ofMinutes(i.longValue())
    def hours: Duration = Duration.ofHours(i.longValue())
    def seconds: Duration = Duration.ofSeconds(i.longValue())
  }

  implicit class IntDateSyntaxSugar(i: Int) {
    def days: Duration = i.longValue().days
    def hours: Duration = i.longValue().hours
    def minutes: Duration = i.longValue().minutes
  }

  implicit class DurationSyntaxSugar(date: Duration) {

    def pretty: String = {
      val days = date.toDays
      val hours = date.toHours
      val minutes = date.toMinutes
      if(days != 0) s"$days days" else
      if(hours != 0) s"$hours hours" else
      if(minutes != 0) s"$minutes minutes" else "--"
    }
  }



  def now: LocalDateTime = LocalDateTime.now

//  implicit class PipeFunCallSyntaxSugar[T, C, E](zio: I => ZIO[C, E, I]) {
//    def recUntil(in: I, condition: O -)
//  }
  implicit class PipeFunCallSyntaxSugar[I](in: I) {
    def =>>[O](f: I => O): O = f(in)
    def tap(f: I => Any): I = {f(in); in}
  }
}
