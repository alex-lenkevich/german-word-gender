package link.german.gender.service

import link.german.gender.LangenscheidtClient

import scala.concurrent.{ExecutionContext, Future}

class DictService extends Service {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val word = msg.capitalize
    LangenscheidtClient.request(word).flatMap(sendBack)
  }

}
