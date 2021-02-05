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

  case class VerbForms(infinitiv: String,
                       person2: String,
                       person3: String,
                       partizip1: String,
                       partizip2: String) {
    override def toString: String = {
      s"""
         |Infinitiv: $infinitiv
         |du: $person2
         |er/sie/es: $person3
         |Partizip I: $partizip1
         |Partizip II: $partizip2""".stripMargin
    }
  }


//  case class VerbForm(thirdForm: String, p1: String, p2: String, )

}
