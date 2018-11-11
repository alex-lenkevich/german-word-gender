package link.german.gender.telegram

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model._
import io.circe.generic.AutoDerivation
import io.circe.syntax._

import scala.concurrent.ExecutionContext


class Client(implicit system: ActorSystem) extends JsonSupport {

  lazy val BotToken: String = System.getenv("BOT_TOKEN")

  def sendMessage(chat: Chat, text: String)(implicit ec: ExecutionContext) = {
    println(s"Sending message '$text' to $chat")
    val body = HttpEntity(SendMessage(chat.id, text).asJson.noSpaces).withContentType(`application/json`)
    val request = HttpRequest(
      uri = s"https://api.telegram.org/bot$BotToken/sendMessage",
      method = HttpMethods.POST,
      entity = body
    )
    println(s"Body: $body")
    println(s"Request: $request")
    Http().singleRequest(request)
  }

}
