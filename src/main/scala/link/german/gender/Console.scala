package link.german.gender

import link.german.gender.service.{GenderService, RouterService}

import scala.concurrent.Future
import scala.io.StdIn
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

object Console /*extends App */{

  while(true) {

    val msg = StdIn.readLine()
    val sendBack: String => Future[Unit] = msg => Future {
      println(msg)
    }
    new GenderService().process(msg, sendBack).onComplete {
      case Success(value) =>
        println(s"Message processed $value")
      case Failure(err) =>
        println(s"Message process Error: ${err.getMessage}")
        err.printStackTrace(System.err)
    }
  }

}
