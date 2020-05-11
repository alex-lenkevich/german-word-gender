package link.german.gender

import java.net.URLEncoder

import link.german.gender.trainer.model.Word
import org.jsoup.Jsoup
import SyntaxSugar._
import io.circe.parser._

import scala.collection.JavaConverters._
import scala.io.Source

trait LingvoClient {

  def readFromLingvo(word: String): Word = {
    val wordUrlEncoded = URLEncoder.encode(word.replaceAll("^(die|der|das) ", "").toUpperCase, "UTF-8")

    val source = Source.fromURL(s"https://api.lingvolive.com/Translation/WordListPart?prefix=${wordUrlEncoded}&srcLang=32775&dstLang=1049&pageSize=10&startIndex=0", "UTF-8")
    val value = parse(source.mkString).right.get
    source.close()

    val url = s"https://www.lingvolive.com/en-us/translate/de-ru/${wordUrlEncoded}"
    val doc = Jsoup.connect(url).get

    val dict = doc.body.selectFirst("*[name=#dictionary]")
//    val name = dict.selectFirst("*._2bepj").text()
//    val headerText = dict.select("p").asScala.map(_.text())
//      .filterNot("".==)
//      .filterNot(_.matches(".*[a-zA-Z\\-<>].*"))
//      .mkString("; ")

    val translate = value.hcursor.downField("items").downArray.get[String]("lingvoTranslations").right.get
    val genus = None //dict.select(" ").asScala.headOption.map(_.text())
    val partOfSpeech = dict.selectFirst("*._24CCn.Zf_4w._3bSyz").text()
    println(translate)


    val examples = doc.body.select("*[name=#quote] ._3CVa3").asScala.map{ quote =>
      val deText = quote.select("._2EqxY > span > *")
        .asScala
        .map(x => if (x.tagName == "strong") s"{{${x.text}}}" else x.text)
        .mkString(" ")
        .replaceAll("  +", " ")

      val ruText = quote.select("._2ITPW").text

      deText -> ruText
    }

    Word(name = word, translate = translate, wordClass = partOfSpeech, examples = Some(examples), genus = genus)
  }

}

object LingvoClient extends ReversoClient with App {
  readFromReverso("Ski") =>> println
}
