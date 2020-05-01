package link.german.gender

import java.time.{Duration, LocalDate, LocalDateTime, OffsetDateTime, Period}
import java.time.temporal.ChronoUnit

object SyntaxSugar {

  implicit val localDateOrdering: Ordering[LocalDate] = (x: LocalDate, y: LocalDate) => x.compareTo(y)
  implicit val localDateTimeOrdering: Ordering[LocalDateTime] = (x: LocalDateTime, y: LocalDateTime) => x.compareTo(y)

  implicit class `collections sugar`[A](val date: TraversableOnce[A]) extends AnyVal {
    def maxOpt(implicit cmp: Ordering[A]): Option[A] = date.reduceOption(cmp.max)
    def minOpt(implicit cmp: Ordering[A]): Option[A] = date.reduceOption(cmp.min)
  }

  implicit class LocalDateTimeSyntaxSugar(date: LocalDateTime) {
    def -(anotherDate: LocalDateTime): Duration = Duration.between(date, anotherDate).abs()
    def +(days: Number): LocalDateTime = date.plusDays(days.longValue())
    def -(days: Number): LocalDateTime = date.minusDays(days.longValue())
    def timeHasCome: Boolean = !now.isBefore(date)
    def past2min: LocalDateTime = if(date.isAfter(LocalDateTime.now)) date else LocalDateTime.MIN

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

  def now: LocalDateTime = LocalDateTime.now

  implicit class PipeFunCallSyntaxSugar[I](in: I) {
    def =>>[O](f: I => O): O = f(in)
    def tap(f: I => Any): I = {f(in); in}
  }
}
