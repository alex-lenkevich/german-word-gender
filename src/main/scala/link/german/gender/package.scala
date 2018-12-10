package link.german

package object model {

  sealed trait Gender {
    def definedArticle: String
  }
  object Masculine extends Gender {
    override def definedArticle: String = "der"
  }
  object Feminine extends Gender {
    override def definedArticle: String = "die"
  }
  object Neuter extends Gender {
    override def definedArticle: String = "das"
  }


//  case class VerbForm(thirdForm: String, p1: String, p2: String, )

}
