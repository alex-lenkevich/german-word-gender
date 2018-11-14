package link.german.gender.service

import scala.concurrent.{ExecutionContext, Future}

class DeclensionService extends Service {

  val Declensions = Seq("N", "A", "D", "G")
  val Genders = Seq("M", "F", "N", "P")

  val personalPronoun2: Map[String, Map[String, String]] = Seq(
    "ich" -> Seq("ich",       "mich"         ,"mir",           "meiner"),
    "du"  -> Seq("du",        "dich"         ,"dir",           "deiner"),
    "Sie" -> Seq("Sie",       "Sie"          ,"Ihnen",         "Ihrer"),
    "er"  -> Seq("er",        "ihn"          ,"ihm",           "seiner"),
    "sie" -> Seq("sie",       "sie"          ,"ihr",           "ihrer"),
    "es"  -> Seq("es",        "es"           ,"ihm",           "seiner"),
    "wir" -> Seq("wir",       "uns"          ,"uns",           "unser"),
    "ihr" -> Seq("ihr",       "euch"         ,"euch",          "euer"),
    "Sie" -> Seq("Sie",       "Sie"          ,"Ihnen",         "Ihrer"),
    "sie" -> Seq("sie",       "sie"          ,"ihnen",         "ihrer")
  ).map {
    case (k, v) =>
      k -> (Declensions zip v).toMap
  }.toMap

  val personalPronoun3: Map[String, Map[String, Map[String, String]]] = Seq(

    "das" -> Seq(
     /*                 m*/  /*f*/  /*n*/  /*M*/
      /*Nominative*/ "der", "die", "das", "die",
      /*Akkusativ */ "den", "die", "das", "die",
      /*Dativ*/      "dem", "der", "dem", "den",
      /*Genetiv*/    "des", "der", "des", "der"
    ),

    "ein" -> Seq(
      /*                m*/    /*f*/    /*n*/
      /*Nominative*/ "ein"  , "eine" , "ein", "",
      /*Akkusativ */ "einen", "eine" , "ein", "",
      /*Dativ*/      "einem", "einer", "einem", "",
      /*Genetiv*/    "eines", "einer", "eines", ""
    ),

    "ich" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "mein"  , "meine" , "mein"  , "meine",
      /*Akkusativ */ "meinen", "meine" , "mein"  , "meine",
      /*Dativ*/      "meinem", "meiner", "meinem", "meinen",
      /*Genetiv*/    "meines", "meiner", "meines", "meiner"
    ),

    "du" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "dein"  , "deine" , "dein"  , "deine",
      /*Akkusativ */ "deinen", "deine" , "dein"  , "deine",
      /*Dativ*/      "deinem", "deiner", "deinem", "deinen",
      /*Genetiv*/    "deines", "deiner", "deines", "deiner"
    ),

    "er" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "sein"  , "seine" , "sein"  , "seine",
      /*Akkusativ */ "seinen", "seine" , "sein"  , "seine",
      /*Dativ*/      "seinem", "seiner", "seinem", "seinen",
      /*Genetiv*/    "seines", "seiner", "seines", "seiner"
    ),

    "sie" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "ihr"   , "ihre" , "ihr"   , "ihre",
      /*Akkusativ */ "ihren" , "ihre" , "ihr"   , "ihre",
      /*Dativ*/      "ihrem" , "ihrer", "ihrem" , "ihren",
      /*Genetiv*/    "ihres" , "ihrer", "ihres" , "ihrer"
    ),

    "es" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "sein"  , "seine" , "sein"  , "seine",
      /*Akkusativ */ "seinen", "seine" , "sein"  , "seine",
      /*Dativ*/      "seinem", "seiner", "seinem", "seinen",
      /*Genetiv*/    "seines", "seiner", "seines", "seiner"
    ),


    "wir" -> Seq(
      /*                m*/       /*f*/       /*n*/       /*M*/
      /*Nominative*/ "unser"   , "unsere" , "unser"   , "unsere",
      /*Akkusativ */ "unseren" , "unsere" , "unser"   , "unsere",
      /*Dativ*/      "unserem" , "unserer", "unserem" , "unseren",
      /*Genetiv*/    "unseres" , "unserer", "unseres" , "unserer"
    ),

    "ihr" -> Seq(
      /*                m*/      /*f*/    /*n*/      /*M*/
      /*Nominative*/ "euer"   , "eure" , "euer"   , "eure",
      /*Akkusativ */ "eueren" , "eure" , "euer"   , "eure",
      /*Dativ*/      "euerem" , "eurer", "euerem" , "eueren",
      /*Genetiv*/    "eueres" , "eurer", "eueres" , "euerer"
    ),

    "Sie" -> Seq(
      /*                m*/     /*f*/    /*n*/     /*M*/
      /*Nominative*/ "Ihr"   , "Ihre" , "Ihr"   , "Ihre",
      /*Akkusativ */ "Ihren" , "Ihre" , "Ihr"   , "Ihre",
      /*Dativ*/      "Ihrem" , "Ihrer", "Ihrem" , "Ihren",
      /*Genetiv*/    "Ihres" , "Ihrer", "Ihres" , "Ihrer"
    )
  ).map {
    case (k, v) => k -> (Declensions zip v.sliding(4, 4).toList.map(Genders zip _))
  }.toMap.mapValues(_.toMap.mapValues(_.toMap))

  override def applicable(msg: String): Boolean = personalPronoun2.contains(msg.split(" ")(0))

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = Future {
    msg.split(" ").toList match {
      case pronoun :: declension :: Nil => personalPronoun2(pronoun)(declension)
      case pronoun :: declension :: gender :: Nil => personalPronoun3(pronoun)(declension)(gender)
    }
  }.flatMap(sendBack)
}