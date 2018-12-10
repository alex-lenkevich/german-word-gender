package link.german.gender

import java.net.URL

import link.german.model._
import org.htmlcleaner.HtmlCleaner

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
    }.map{
      gender =>
        println(s"Gender for $word is $gender")
        gender
    }
  }

}
