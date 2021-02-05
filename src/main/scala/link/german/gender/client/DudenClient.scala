package link.german.gender.client

import java.net.URL

import org.htmlcleaner.HtmlCleaner
import org.jsoup.Jsoup
import zio._
import zio.console._

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration._
import scala.util.Try

trait DudenClient {

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

  def dudenRequestNounForms(word: String): Task[(String, String)] = Task {
    println(word)


    val url = Jsoup.connect(s"https://www.duden.de/suchen/dudenonline/${word}").get.body.selectFirst("a.vignette__label").attr("href")
    val doc = Jsoup.connect(s"https://www.duden.de$url").get
    Try {
      val plural = doc.body().select("tbody.wrap-table__flexion > tr:nth-child(1) > td:nth-child(3)").text().substring(4)
      val genetiv = doc.body().select("tbody.wrap-table__flexion > tr:nth-child(2) > td:nth-child(2)").text().substring(4)
      genetiv -> plural
    }.getOrElse {
      val grammaText = doc.body().select("*#grammatik p").text().split(";")(1).split(",")
      val grammatik = grammaText.map(_.split(":", 2).map(_.trim)).collect {
        case Array(a, b) => a -> b
      }.toMap
      val plural = grammatik.getOrElse("Plural", grammaText(1).trim)
      val genetiv = grammatik("Genitiv")
      genetiv -> plural
    }
  }

}

object DudenClient extends App with DudenClient {


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] =
    (for {
      res <- dudenRequestNounForms("Magen")
      _ <- putStrLn(res.toString())
    } yield 0).orDie
}

