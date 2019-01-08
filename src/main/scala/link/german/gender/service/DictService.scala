package link.german.gender.service

import link.german.gender.{LangenscheidtClient, WikiClient}

import scala.concurrent.{ExecutionContext, Future}

class DictService extends Service with TableOutput {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](word: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    WikiClient.request(word).flatMap {
      case Some(text) => Future.successful(print3dimension(text))
      case None => LangenscheidtClient.request(word)
    }.flatMap(sendBack)
  }

}
