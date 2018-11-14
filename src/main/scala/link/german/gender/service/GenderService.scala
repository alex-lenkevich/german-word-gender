package link.german.gender.service

import link.german.gender.BablaClient

import scala.concurrent.{ExecutionContext, Future}

class GenderService extends Service {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val word = msg.capitalize
    BablaClient.requestGender(word).map {
      _.map(_.definedArticle)
    }.flatMap { x =>
      sendBack(s"${x.getOrElse("???")} $word")
    }
  }

}
