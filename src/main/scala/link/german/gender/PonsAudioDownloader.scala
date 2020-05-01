package link.german.gender

import java.io.File
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest}
import java.net.{URI, URL, URLEncoder}

import org.htmlcleaner.HtmlCleaner

trait PonsAudioDownloader {

  val cleaner = new HtmlCleaner

  def getPonsAudio(wordRaw: String) = {
    val word = wordRaw.toLowerCase.replaceAll("^(die|das|der) ", "")
    val wordSpaceless = word.replaceAll("\\s+", "_")
    val targetPath = s"/Users/oleksandr.linkevych/Projects/german-word-gender/files/audio/$wordSpaceless.mp3"
    val file = new File(targetPath)
    if(!file.exists()) {
      val root = cleaner.clean(new URL(s"https://en.pons.com/translate/german-russian/${URLEncoder.encode(word, "UTF-8")}"))
      val id = root.evaluateXPath(s"//*[@data-translation][//*[@class='headword']]/@id").toSeq.head.toString

      val client = HttpClient.newHttpClient
      val request = HttpRequest.newBuilder.uri(URI.create(s"https://sounds.pons.com/audio_tts/de/$id")).build
      val response = client.send(request, BodyHandlers.discarding())
      val redirect = response.headers().firstValue("location")
      val audioRequest = HttpRequest.newBuilder.uri(URI.create(redirect.get)).build
      client.send(audioRequest, BodyHandlers.ofFile(file.toPath))
    }
    targetPath
  }

}
