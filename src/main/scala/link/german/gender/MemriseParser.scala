package link.german.gender

import java.io.FileWriter
import java.net.URL

import org.htmlcleaner.HtmlCleaner

object MemriseParser extends App {

  private val output = new FileWriter("verbs.tsv")

  val cleaner = new HtmlCleaner
  (1 to 1).foreach { page =>
    println(page)
    val root = cleaner.clean(new URL(s"https://www.memrise.com/course/5701712/my-way-to-german-vb/"))
    root.getElementsByAttValue("class", "thing text-text", true, true).foreach { line =>
      val word = line.findElementByAttValue("class", "col_a col text", true, true).getText
      val desc = line.findElementByAttValue("class", "col_b col text", true, true).getText
      output.write(Seq(word,desc).mkString("", "\t", "\n"))
    }
  }

  output.close()

}
