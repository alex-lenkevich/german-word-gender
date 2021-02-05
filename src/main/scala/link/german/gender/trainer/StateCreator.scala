package link.german.gender.trainer

import java.io.FileWriter

import io.circe.syntax._
//import link.german.gender.Dict02Parser.getArticle
import link.german.gender.trainer.model._
import link.german.gender.SyntaxSugar._

import scala.io.Source

object StateCreator extends App {
  private val writer = new FileWriter("trainer_state.json")
  Source.fromFile("b1_words_ru3.tsv").getLines()
    .map(_.split("\t", 8).map(_.trim).padTo(8, "")).collect {
    case Array(searchWord, word, wordClass, translations, verbClass, genus, info, flexion) =>
      searchWord -> Word(word, translations,
        wordClass,
        Some(flexion).filterNot("".==),
        Some(genus).filterNot("".==),
        Some(verbClass).filterNot("".==),
        Some(info).filterNot("".==))
    case a =>
      println(a.mkString("|"))
      throw new Exception()
  }.toList.groupBy(_._1).mapValues(_.map(_._2)).map { case (searchWord, transations) =>
    transations.map(x => x.copy(name = x.name.replaceAll("\\(.*\\)", "")
      .replaceAll(" {2}", " ").trim))
      .filter(_.name == searchWord)
  }
    .filterNot(_.isEmpty)
    .toSeq
//    .filter { x =>
      //      x.head.wordClass == "VB"
      //      x.head.wordClass == "ADV"
//      x.head.wordClass == "N"
      //      Seq("CONJ", "ADJ", "ADV").contains(x.head.wordClass)
//    }
    .sortBy( x =>
      (
        x.head.genus,
        x.head.name.toLowerCase.replaceAll("^(be|ge|ent|emp|er|ver|miß|zer|los|an|ab|bei|mit|ein|vor|nach|uber|über|zu|unter|um|aus|auf)+", ""),
        x.head.name.toLowerCase.replaceAll("^((be|ge|ent|emp|er|ver|miß|zer|los|an|ab|bei|mit|ein|vor|nach|uber|über|zu|unter|um|aus|auf)*)", "$1_"),
      )
    )
    .map { w =>
      w.head.copy(name = /*getArticle(w.head)*/None.fold(w.head.name)(x => s"$x ${w.head.name}"),
        translate = (w match {
          case Seq(x) => x.translate
          case translations =>
            translations.map(_.translate).distinct
              .filterNot(x => x.startsWith("-"))
              .zipWithIndex.map {
              case (t, i) => s"${i + 1}. $t"
            }.mkString("; ")
        }).replaceAll("[a-z]+", "")
          .replaceAll("perf", "")
          .replaceAll("  +", " ")
          .replaceAll("\\s*;\\s*", "; ")
          .replaceAll("\\s*-[^ ;]+\\s*", " ")
          .replaceAll("\\s*[^ ;]+-\\s*", " ")
          .replaceAll("(;\\s+)+;", ";")
          .replaceAll(";\\s*$", ""),
      )
    }.map(WordState(_)).asJson.noSpaces =>> writer.write
  writer.close()
}
