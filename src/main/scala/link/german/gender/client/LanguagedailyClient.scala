package link.german.gender.client

import java.net.URL

import org.htmlcleaner.HtmlCleaner

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

object LanguagedailyClient extends App {

  val urls =
    Seq(
      "http://www.languagedaily.com/learn-german/vocabulary/common-german-words",
      "http://www.languagedaily.com/learn-german/vocabulary/most-common-german-words-2"
    ) ++:
    (3 to 12).map(i => s"http://www.languagedaily.com/learn-german/vocabulary/common-german-words-$i")

  def request = {
    val cleaner = new HtmlCleaner
    urls.toList.flatMap { url =>
      val root = cleaner.clean(new URL(url))
      root.getElementsByAttValue("class", "bigLetter", true, true).map {
        _.getText.toString.split(" ")(0)
      }.toList
    }.map { x =>
      println(s"Process $x")
      val (group, text) = Await.result(BablaClient.requestVerb(x).flatMap {
        case Some(value) => Future.successful("verb" ->
          s"${value.infinitiv}|${value.person2}|${value.person3}|${value.partizip1}|${value.partizip2}")
        case None => BablaClient.requestGender(x).map {
          case Some(value) => "noun" -> s"${value.definedArticle} $x"
          case None => "other" -> x
        }
      }, 10 seconds)
      println(s"$group => $text")
      group -> text
    }
      .groupBy(_._1)
      .foreach {
      case (key, values) =>
        println(s"============ $key =============")
        values.foreach(println)
    }

  }

  request


}
