package link.german.gender.service

import scala.concurrent.{ExecutionContext, Future}

trait Service {

  def applicable(msg: String): Boolean
  def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T]

}
