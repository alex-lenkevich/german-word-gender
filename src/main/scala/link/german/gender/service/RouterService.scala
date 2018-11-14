package link.german.gender.service
import scala.concurrent.{ExecutionContext, Future}

class RouterService extends Service {

  private val services = Seq(new DeclensionService, new GenderService)

  override def applicable(msg: String): Boolean = true

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    services.find(_.applicable(msg)).get.process(msg, sendBack)
  }
}
