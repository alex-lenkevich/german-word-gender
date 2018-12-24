package link.german.gender.service

import link.german.gender.{LangenscheidtClient, WikiClient}

import scala.concurrent.{ExecutionContext, Future}

class DictService extends Service with TableOutput {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val word = msg.capitalize
    WikiClient.request(word).flatMap {
      case Some(text) => Future.successful(print3dimension(text))
      case None => LangenscheidtClient.request(word)
    }.flatMap(sendBack)
  }

}
