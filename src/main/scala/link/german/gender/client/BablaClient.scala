package link.german.gender.client

import java.net.URL

import link.german.model._
import org.htmlcleaner.{HtmlCleaner, TagNode}

import scala.concurrent.{ExecutionContext, Future}

object BablaClient {

  def requestGender(word: String)(implicit ec: ExecutionContext): Future[Option[Gender]] = Future {
    val cleaner = new HtmlCleaner
    val root = cleaner.clean(new URL(s"https://de.bab.la/woerterbuch/deutsch-englisch/$word"))
    root.getElementsByAttValue("class", "suffix", true, true).find {
      x => String.valueOf(x.getText).matches("\\{(Neutrum|Maskulin|Feminin)\\}")
    }.map(x => String.valueOf(x.getText).trim).map {
      case "{Neutrum}" => Neuter
      case "{Maskulin}" => Masculine
      case "{Feminin}" => Feminine
    }
  }

  implicit class `implicit for TagNode`(root: TagNode) {
    def findElementByClass(className: String) = {
      root.getElementsByAttValue("class", className, true, true)
    }
  }

  def requestVerb(word: String)(implicit ec: ExecutionContext): Future[Option[VerbForms]] = Future {
    val cleaner = new HtmlCleaner
    val root = cleaner.clean(new URL(s"https://de.bab.la/konjugieren/deutsch/$word"))
    def getValueByKey(title: String, classes: (String, String, String)) = {
      val (topClass, labelClass, valueClass) = classes
      root.findElementByClass(topClass).find {
        _.findElementByClass(labelClass).exists(_.getText.toString.trim == title)
      }.map(_.findElementByClass(valueClass).map(_.getText.toString.trim).toList)
    }.getOrElse(Seq()).mkString("; ")

    val PartizipClasses = ("quick-result-entry", "quick-result-option", "sense-group-results")
    val ConjPersonClasses = ("conj-item", "conj-person", "conj-result")

    val infinitiv = getValueByKey("Infinitiv", PartizipClasses)
    if (infinitiv == "") {
      None
    } else {
      Some(VerbForms(
        infinitiv,
        getValueByKey("du", ConjPersonClasses),
        getValueByKey("er/sie/es", ConjPersonClasses),
        getValueByKey("Partizip Präsens", PartizipClasses),
        getValueByKey("Partizip Perfekt", PartizipClasses)
      ))
    }
  }

}
