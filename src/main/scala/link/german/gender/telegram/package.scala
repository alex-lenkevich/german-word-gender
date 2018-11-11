package link.german.gender

package object telegram {

  case class Update(message: Message)
  case class Message(chat: Chat, text: String)
  case class Chat(id: Long)

  case class SendMessage(chatId: Long, text: String)

}
