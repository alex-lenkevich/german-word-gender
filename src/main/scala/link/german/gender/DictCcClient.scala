package link.german.gender

import java.util.UUID

import link.german.gender.trainer2.model.{Kasus, WordData, WordType}
import org.jsoup.Jsoup
import zio._
import zio.console._

import scala.collection.JavaConverters._

trait DictCcClient {

  def dictCcClientGetWord(word: String): Task[String] = Task {
//    val body = Jsoup.connect(s"https://deru.dict.cc/?s=$word").get.body
//    val table = body.selectFirst("#maincontent > table:nth-child(7) > tbody > tr:nth-child(2) > td:nth-child(2) > table")
//      table.select("tr[title]")
//        .asScala
//        .map { e =>
//          e.attr("title") match {
//            case "article sg | article pl" =>
//              val data = e.selectFirst("td:nth-child(2)").text
//              WordData(id = UUID.randomUUID().toString,
//                de = e.selectFirst("td:nth-child(2)"),
//                ru = "",
//                `type` = Some(WordType.Noun),
//                kasus = Some {
//                  if(data.trim.matches("^der")) Kasus.M
//                  else if(data.trim.matches("^die")) Kasus.F
//                }
//              )
//          }
//        }
//    doc.toString()
    ""
  }

}

object DictCcClient extends DictCcClient with App {

  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = (for {
    a <- dictCcClientGetWord("Äpfel") <&>
      dictCcClientGetWord("fördern") <&>
      dictCcClientGetWord("deshalb")
    _ <- putStrLn(a.toString)
  } yield 0).orDie
}



