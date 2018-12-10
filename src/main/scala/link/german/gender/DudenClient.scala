package link.german.gender

import java.net.URL

import org.htmlcleaner.HtmlCleaner

import scala.concurrent.{ExecutionContext, Future}

object DudenClient {

  def requestGender(word: String)(implicit ec: ExecutionContext): Future[Seq[(String, String, String, String)]] = Future {
    val cleaner = new HtmlCleaner
    val root = cleaner.clean(new URL(s"https://www.duden.de/rechtschreibung/$word"))
    root.getElementsByAttValue("class", "wide", true, true).find {
      _.getElementsByName("table", true).nonEmpty
    }.map { table =>
      val columns = table.getElementsByName("thead", true)(0).getElementsByName("th", true).map(_.getText.toString.trim)
      table.getElementsByName("tbody", true)(0).getElementsByName("tr", true).zipWithIndex.flatMap { case (tr, i) =>
        val rowLabel = tr.getElementsByName("th", true)(0).getText.toString.trim
        val rowValues = tr.getElementsByName("td", true).map(_.getText.toString.trim)
        (columns.tail zip rowValues).map { case (column, value) => ("", s"$i:$rowLabel", column, value)}
      }.toSeq
    }.getOrElse(Seq[(String, String, String, String)]())
  }

}
