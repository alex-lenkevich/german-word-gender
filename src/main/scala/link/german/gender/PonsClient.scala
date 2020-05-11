package link.german.gender

import java.net.{URL, URLEncoder}

import link.german.gender.trainer.model.Word
import org.htmlcleaner.HtmlCleaner

object PonsClient {

  val cleaner = new HtmlCleaner

  def readFromPons(word: String) = {
    val url = s"https://en.pons.com/translate/german-russian/${URLEncoder.encode(word, "UTF-8")}"
    println(url)
    val root = cleaner.clean(new URL(url))
    val words = root.getElementsHavingAttribute("rel", true).flatMap { line =>
      val translations = line.getElementsByName("dl", true).map {
        translate =>
          val source = translate.findElementByAttValue("class", "source", true, true).getText.toString.trim
          val target = translate.findElementByAttValue("class", "target", true, true).getText.toString.trim
          source.replaceAll("\n", ";").replaceAll("\t", " ") -> target.replaceAll("\n", ";").replaceAll("\t", " ")}
        .groupBy(_._1).mapValues(_.map(_._2).mkString(";"))
      val wordClass = line.getElementsByAttValue("class", "wordclass", true, true).headOption.map(_.getText.toString.trim)
      val flexion = line.getElementsByAttValue("class", "flexion", true, true).headOption.map(
        _.getText.toString.trim.replaceAll("&lt;", "<").replaceAll("&gt;", ">")
          .replaceAll("\n", ";")
          .replaceAll("\t", " ")
      )
      val genus = line.getElementsByAttValue("class", "genus", true, true).headOption.map(_.getText.toString.trim)
      val verbClass = line.getElementsByAttValue("class", "verbclass", true, true).headOption.map(_.getText.toString.trim)
      val info = line.getElementsByAttValue("class", "info", true, true).headOption.map(_.getText.toString.trim)
      translations.map{case (label, translate) => (label, translate, wordClass, flexion, genus, verbClass, info)}
    }
      .collect { case (wordName, translations, wordClass, flexion, genus, verbClass, info) =>
        Word(wordName, translations, wordClass.getOrElse("N/A"), flexion, genus, verbClass, info)
      }
    if (words.isEmpty) {
      println(s"Not found $word")
    }
    words.toList
  }

}
