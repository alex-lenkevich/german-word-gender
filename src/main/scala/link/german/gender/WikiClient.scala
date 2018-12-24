package link.german.gender

import java.net.URL

import org.htmlcleaner.{ContentNode, HtmlCleaner, TagNode}

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.JavaConverters._

object WikiClient {

  def request(word: String)(implicit ec: ExecutionContext): Future[Option[Seq[(String, String, String, String)]]] = Future {
    val cleaner = new HtmlCleaner
    val root = cleaner.clean(new URL(s"https://de.wiktionary.org/wiki/$word"))
    root.getElementsByAttValue("class", "wikitable float-right inflection-table flexbox hintergrundfarbe2", true, true).
      headOption.
      map { table =>
        val tbody = table.getElementsByName("tbody", true)(0)
        val columns = tbody.getElementsByName("tr", true)(0).getAllElements(false).map(_.getText.toString.trim)
        tbody.getElementsByName("tr", true).zipWithIndex.flatMap { case (tr, i) =>
          val rowLabel = tr.getElementsByName("th", true)(0).getText.toString.trim
          val rowValues = tr.getElementsByName("td", true).map{x =>
            x.getAllChildren.asScala.map {
              case x: TagNode if x.getName == "br" => "\n"
              case x: ContentNode => x.getContent.trim
              case x: TagNode => x.getText.toString.trim
            }.mkString(" ").replaceAll("\\s*\\n", ", ").replaceAll(" \\s+", " ")
          }
          (columns.tail zip rowValues).map { case (column, value) => ("", s"$rowLabel", column, value) }
        }.toSeq
      }
  }

}
