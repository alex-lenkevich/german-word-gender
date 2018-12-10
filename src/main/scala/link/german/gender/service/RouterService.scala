package link.german.gender.service
import scala.concurrent.{ExecutionContext, Future}

class RouterService extends Service {

  private val services = Seq(new DeclensionService, /*new GenderService, */ new DictService)

  override def applicable(msg: String): Boolean = true

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    Future {
      services.find(_.applicable(msg))
    }.flatMap(_.fold[Future[T]](Future.failed(new RuntimeException("Can't find service")))(_.process(msg, sendBack)))
  }
}
