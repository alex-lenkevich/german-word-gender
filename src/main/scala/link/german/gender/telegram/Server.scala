package link.german.gender.telegram

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport
import link.german.gender.service.{GenderService, RouterService}

import scala.util.{Failure, Success}

object Server extends JsonSupport with App with ErrorAccumulatingCirceSupport {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher
  private val telegramClient = new Client()
  private val servcieRouter = new GenderService()

  val route = (path("update") & post & entity(as[Update])) { update =>
    val word = update.message.text
    if (word == "/start") {
      telegramClient.sendMessage(update.message.chat, s"Herzlich willkommen. Send me a noun and I'll suggest its gender.")
    } else {
      println(s"Received message $word")
      servcieRouter.process(word, answer => telegramClient.sendMessage(update.message.chat, answer))
    }.onComplete {
      case Success(value) =>
        println(s"Message processed $value")
      case Failure(err) =>
        println(s"Message process Error: ${err.getMessage}")
        err.printStackTrace(System.err)
    }
    complete("")
  }

  private val port = System.getenv("PORT").toInt
  println(s"Binding to $port")

  Http().bindAndHandle(route, "0.0.0.0", port).onComplete {
    case Success(value) =>
      println("Server Successfully Started: " + value)
    case Failure(err) =>
      println(s"Server Start Error: ${err.getMessage}")
      err.printStackTrace(System.err)
  }

}
