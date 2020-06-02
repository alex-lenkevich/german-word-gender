package link.german.gender.trainer2

import link.german.gender.trainer2.model.{Answer, WordListLike, WordTestResults}
import org.apache.commons.text.similarity.{LevenshteinDetailedDistance, LevenshteinDistance}

object test {

  case class Test(`type`: TestType, form: TestMethod)

  trait TestType {
    def isSupported(word: WordTestResults): Boolean
    def question(word: WordTestResults): String
    def answer(word: WordTestResults): String
    def options(word: WordTestResults, list: WordListLike, n: Int): Seq[String] = ???
  }

  object TestType {

    case object Translate extends TestType {
      def question(word: WordTestResults): String = word.data.ru

      def answer(word: WordTestResults): String = word.data.kasus match {
        case Some(kasus) => s"${kasus.article} ${word.data.de}"
        case None => word.data.de
      }

      override def isSupported(word: WordTestResults): Boolean = true

      override def options(word: WordTestResults, list: WordListLike, n: Int): Seq[String] =
        list.states.sortBy(s => LevenshteinDistance.getDefaultInstance()(word.data.de, s.data.de)).take(n).map(this.answer)
    }

    sealed trait VerbTestType extends TestType {

      override def isSupported(word: WordTestResults): Boolean = word.data.perfekt.isDefined
    }

    case object Present3 extends VerbTestType {
      def question(word: WordTestResults): String = s"${word.data.de} (Present form 3)"
      def answer(word: WordTestResults): String =  word.data.present3.get
    }

    case object Prateritum extends VerbTestType {
      def question(word: WordTestResults): String = s"${word.data.de} (Präteritum)"
      def answer(word: WordTestResults): String =  word.data.prateritum.get

    }

    case object Perfekt extends VerbTestType {
      def question(word: WordTestResults): String = s"${word.data.de} (Perfekt)"
      def answer(word: WordTestResults): String =  word.data.perfekt.get
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
