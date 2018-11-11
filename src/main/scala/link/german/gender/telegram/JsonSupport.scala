package link.german.gender.telegram
import io.circe.{Decoder, Encoder}
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._

trait JsonSupport {
  implicit val circeConfiguration: Configuration = Configuration.default.withSnakeCaseMemberNames.withDiscriminator("type")

  implicit val updateDecoder: Decoder[Update] = deriveDecoder
  implicit val updateEncoder: Encoder[Update] = deriveEncoder

  implicit val messageDecoder: Decoder[Message] = deriveDecoder
  implicit val messageEncoder: Encoder[Message] = deriveEncoder

  implicit val chatDecoder: Decoder[Chat] = deriveDecoder
  implicit val chatEncoder: Encoder[Chat] = deriveEncoder

  implicit val sendMessageDecoder: Decoder[SendMessage] = deriveDecoder
  implicit val sendMessageEncoder: Encoder[SendMessage] = deriveEncoder

}
