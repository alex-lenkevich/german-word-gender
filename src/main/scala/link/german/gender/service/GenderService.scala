package link.german.gender.service

import link.german.gender.DudenClient

import scala.concurrent.{ExecutionContext, Future}

class GenderService extends Service with TableOutput {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val word = msg.capitalize
    DudenClient.requestGender(word).map(print3dimension).flatMap(sendBack)
  }

}
