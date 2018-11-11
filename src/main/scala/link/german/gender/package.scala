package link.german

package object gender {

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

}
