package link.german.gender.service

import link.german.gender.client._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

class GenderService extends Service with TableOutput {

  override def applicable(msg: String): Boolean = !msg.contains(" ")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    Future.sequence(Seq(
      BablaClient.requestGender(msg.capitalize).map(_.map(gender => s"${gender.definedArticle} ${msg.capitalize}")),
      BablaClient.requestVerb(msg.toLowerCase()).map(_.map(_.toString)),
      WikiClient.request(msg.capitalize).map(_.map(x => print3dimension(x))),
      WikiClient.request(msg.toLowerCase).map(_.map(x => print3dimension(x))),
//      DudenClient.requestGender(msg.capitalize).map(print3dimension),
//      DudenClient.requestGender(msg.toLowerCase).map(print3dimension),
//      LangenscheidtClient.request(msg.capitalize),
//      LangenscheidtClient.request(msg.toLowerCase),
    ).map(_.recover {
      case NonFatal(_) => None
    })).map(_.flatten.mkString("\n-------------------------\n")).flatMap(sendBack)
  }

}
