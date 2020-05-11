package link.german.gender

import java.net.URLEncoder

import io.circe.parser._
import link.german.gender.trainer.model.Word
import org.jsoup.Jsoup

import scala.collection.JavaConverters._
import scala.io.Source

trait ReversoClient {

  def readFromReverso(word: String) = {
    val wordUrlEncoded = URLEncoder.encode(word.replaceAll("^(die|der|das) ", "").toUpperCase, "UTF-8")
//
//    val definitionUrl = s"https://dictionary.reverso.net/german-definition/$wordUrlEncoded"
//    val contextUrl = s"https://context.reverso.net/%D0%BF%D0%B5%D1%80%D0%B5%D0%B2%D0%BE%D0%B4/%D0%BD%D0%B5%D0%BC%D0%B5%D1%86%D0%BA%D0%B8%D0%B9-%D1%80%D1%83%D1%81%D1%81%D0%BA%D0%B8%D0%B9/$wordUrlEncoded"
//
//    val definitionDoc = Jsoup.connect(definitionUrl).get
//    val contextUrl = Jsoup.connect(contextUrl).get
//
//    val genus = Option(definitionDoc.body.getElementById("ID0ENB")).map(_.text() match {
//      case x if x.contains() =>
//    })

//    val value = parse(source.mkString).right.get
//    source.close()
//
//    val url = s"https://www.lingvolive.com/en-us/translate/de-ru/${wordUrlEncoded}"
//    val doc = Jsoup.connect(url).get
//
//    val dict = doc.body.selectFirst("*[name=#dictionary]")
////    val name = dict.selectFirst("*._2bepj").text()
////    val headerText = dict.select("p").asScala.map(_.text())
////      .filterNot("".==)
////      .filterNot(_.matches(".*[a-zA-Z\\-<>].*"))
////      .mkString("; ")
//
//    val translate = value.hcursor.downField("items").downArray.get[String]("lingvoTranslations").right.get
//    val genus = None //dict.select(" ").asScala.headOption.map(_.text())
//    val partOfSpeech = dict.selectFirst("*._24CCn.Zf_4w._3bSyz").text()
//    println(translate)
//
//
//    val examples = doc.body.select("*[name=#quote] ._3CVa3").asScala.map{ quote =>
//      val deText = quote.select("._2EqxY > span > *")
//        .asScala
//        .map(x => if (x.tagName == "strong") s"{{${x.text}}}" else x.text)
//        .mkString(" ")
//        .replaceAll("  +", " ")
//
//      val ruText = quote.select("._2ITPW").text
//
//      deText -> ruText
//    }
//
//    Word(name = word, translate = translate, wordClass = partOfSpeech, examples = Some(examples), genus = genus)
  }

}

object ReversoClient extends App with ReversoClient {

  readFromReverso("Tätigkeit")

}


