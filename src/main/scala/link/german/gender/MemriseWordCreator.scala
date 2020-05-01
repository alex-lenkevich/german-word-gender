package link.german.gender

import java.time.Duration

import org.openqa.selenium.By
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.support.ui.WebDriverWait

import scala.collection.JavaConverters._
import scala.util.Try
import scala.util.control.NonFatal
import sys.process._

object MemriseWordCreator extends App with PonsAudioDownloader {

  val forceRemove = false

  var driver = new ChromeDriver()
  private val wait_ = new WebDriverWait(driver, Duration.ofSeconds(20))

  driver.get("https://www.memrise.com/login/")
  driver.findElementByXPath("//*[@aria-label=\"allow cookies\"]").click()
  driver.findElementByXPath("//*[@data-testid=\"loginUsernameInput\"]").sendKeys("alex.lenkevich@gmail.com")
  driver.findElementByXPath("//*[@data-testid=\"loginPasswordInput\"]").sendKeys("8GzbVMD42")
  wait_.until(_ => driver.findElementByXPath("//*[@data-testid=\"loginFormSubmit\"]").isDisplayed)
  driver.findElementByXPath("//*[@data-testid=\"loginFormSubmit\"]").click()

  Seq(
    "https://www.memrise.com/course/5714696/my-way-to-german-n/edit/#l_12518539",
    "https://www.memrise.com/course/5714696/my-way-to-german-n/edit/#l_12518541",
    "https://www.memrise.com/course/5714696/my-way-to-german-n/edit/#l_12518555"
  ).foreach { url =>

    driver.get(url)
    wait_.until(_ => driver.findElementByClassName("things").isDisplayed)
    driver.findElementsByClassName("thing").asScala.foreach { line =>
      try {
        def fileElement() = line.findElement(By.xpath(".//td[4]//button"))

        def uploadFile = {
          val text = line.findElement(By.xpath(".//td[2]")).getText
          println(text)
          val path = getPonsAudio(text)
          line.findElement(By.xpath(".//*[@class=\"add_thing_file\"]")).sendKeys(path)
        }

        if (fileElement().getText.trim.contains("файл audio отсутствует")) Try {
          uploadFile
        } else if (forceRemove) {
          fileElement().click()
          line.findElement(By.className("ico-trash")).click()
          uploadFile
        }
      } catch {
        case NonFatal(e) => e.printStackTrace()
      }
    }
    driver.findElementByXPath("//*[@id=\"content\"]/div/div[3]/a").click()
  }

  driver.close()

  //
  //
  //  def createSound(word: String): String = {
  //    val wordSpaceless = word.replaceAll("\\s+", "_")
  //    val trackPath = s"/Users/oleksandr.linkevych/Projects/german-word-gender/files/audio/$wordSpaceless.aiff"
  //    val targetPath = s"/Users/oleksandr.linkevych/Projects/german-word-gender/files/audio/$wordSpaceless.mp3"
  //    val sayCmd = s"""say "$word" -o $trackPath -v Anna -r 150"""
  //    val convertCmd = s"""lame -m m $trackPath $targetPath"""
  //    val rmCmd = s"""rm -f $trackPath"""
  //    assert(sayCmd.! == 0)
  //    assert(convertCmd.! == 0)
  //    assert(rmCmd.! == 0)
  //    targetPath
  //  }

}
