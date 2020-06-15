package link.german.gender.trainer2

import link.german.gender.trainer2.model.{Answer, WordData}
import org.apache.commons.text.similarity.LevenshteinDistance

object test {

  case class Test(`type`: TestType, form: TestMethod)

  trait TestType {
    def isSupported(word: WordData): Boolean
    def question(word: WordData): String
    def answer(word: WordData): String
    def options(word: WordData, list: Seq[WordData], n: Int): Seq[String] = ???
    def order = "5"
  }

  object TestType {

    case object Translate extends TestType {
      def question(word: WordData): String = word.ru

      def answer(word: WordData): String = word.kasus match {
        case Some(kasus) => s"${kasus.article} ${word.de}"
        case None => word.de
      }

      override def order: String = "0"

      override def isSupported(word: WordData): Boolean = true

      override def options(word: WordData, list: Seq[WordData], n: Int): Seq[String] =
        (list.map(_.de) :+ answer(word)).distinct.sortBy(s => LevenshteinDistance.getDefaultInstance()(answer(word), s)).take(n)
    }

    case object KasusTestType extends TestType {
      def question(word: WordData): String = word.de

      def answer(word: WordData): String = word.kasus match {
        case Some(k) => k.article
      }

      override def isSupported(data: WordData): Boolean = data.kasus.isDefined

      override def options(word: WordData, list: Seq[WordData], n: Int): Seq[String] =
        Seq("die", "der", "das")
    }

    sealed trait VerbTestType extends TestType {

      override def isSupported(data: WordData): Boolean = data.perfekt.isDefined
    }

    case object Present3 extends VerbTestType {
      def question(word: WordData): String = s"${word.de} (Present form 3)"
      def answer(word: WordData): String =  word.present3.get
    }

    case object Prateritum extends VerbTestType {
      def question(word: WordData): String = s"${word.de} (Präteritum)"
      def answer(word: WordData): String =  word.prateritum.get

    }

    case object Perfekt extends VerbTestType {
      def question(word: WordData): String = s"${word.de} (Perfekt)"
      def answer(word: WordData): String =  word.perfekt.get
    }

  }

  sealed trait TestMethod {
    def isCorrect(answer: Answer): Boolean
  }

  object TestMethod {
    object Text extends TestMethod {
      override def isCorrect(answer: Answer): Boolean = answer.correct && answer.attempts.length <= 4
    }
    object Select extends TestMethod {
      override def isCorrect(answer: Answer): Boolean = answer.correct && answer.attempts.length <= 1
    }
  }


}
