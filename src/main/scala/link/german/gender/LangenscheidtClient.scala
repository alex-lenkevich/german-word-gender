package link.german.gender

import java.net.URL

import org.htmlcleaner.{ContentNode, HtmlCleaner, TagNode}

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.JavaConverters._

object LangenscheidtClient {

  val text: TagNode => String = _.getText.toString.trim

  def request(word: String)(implicit ec: ExecutionContext): Future[String] = Future {
    val cleaner = new HtmlCleaner
    val root = cleaner.clean(new URL(s"https://de.langenscheidt.com/deutsch-englisch/$word"))
    val terms = findByClass(root, "search-term").map { t =>
      Seq(
        t.getElementsByName("h5", true).headOption.map(_.getAllChildren.asScala).flatMap(_.collectFirst {
          case x: ContentNode => x.getContent
        }).getOrElse(word),
        t.getElementsByName("abbr", true).headOption.map(text).getOrElse("--")
      ).mkString(": ")
    }
//    val translate = findByClass(root, "summary-inner").map( i =>
//      findByClass(i, "btn-inner").map(text).mkString("; ")
//    )
//    (terms zip translate).map(x => x._1 + "\n" + x._2).
    terms.
      filterNot(_.contains("Ü")).
      mkString("\n").replaceAll("&lt;", "<").replaceAll("&gt;", ">").replaceAll(" {2,}", " ")
  }

  private def findByClass(table: TagNode, trans: String) = {
    table.getElementsByAttValue("class", trans, true, true)
  }
}
