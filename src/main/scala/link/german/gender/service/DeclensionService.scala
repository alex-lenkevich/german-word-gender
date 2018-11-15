package link.german.gender.service

import scala.concurrent.{ExecutionContext, Future}

class DeclensionService extends Service {

  val Declensions = Seq("N", "A", "D", "G")
  val Genders = Seq("M", "F", "N", "P")
  val Regex = "(\\(\\)|\\s)*(->|'| (to|in) | )(\\(\\)|\\s)*"

  val personalPronoun2: Seq[(String, String, String)] = Seq(
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
  ).flatMap {
    case (k, v) => (Declensions zip v).map{case (d, x) => (k, d, x)}
  }

  val personalPronoun3: Seq[(String, String, String, String)] = Seq(

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
    ),
    "adj" -> Seq(
      /*                m*/  /*f*/  /*n*/  /*M*/
      /*Nominative*/ "-er" , "-e" , "-es", "-e",
      /*Akkusativ */ "-en" , "-e" , "-es", "-e",
      /*Dativ*/      "-em" , "-er", "-em", "-en",
      /*Genetiv*/    "-en" , "-er", "-en", "-er"
    )
  ).flatMap {
    case (k, v) =>
      for {
        t2 <- (Declensions zip v.sliding(4, 4).toList).toList
        t3 <- (Genders zip t2._2).toList
      } yield {
        (k, t2._1, t3._1, t3._2)
      }
  }

  val Pronouns = personalPronoun2.map(_._1) ++ personalPronoun3.map(_._1)

  override def applicable(msg: String): Boolean = msg.matches(".*" + Regex + ".*")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = Future {
    msg.split(Regex).toList match {
      case pronoun :: declension :: Nil =>
        personalPronoun2.collect {
          case (p, d, v) if (p == pronoun || pronoun == "*") &&
            (d == declension.toUpperCase || declension == "*") =>
            ("", p, d, v)
        }
      case pronoun :: gender :: declension :: Nil =>
        personalPronoun3.collect {
        case (p, d, g, v) if (p == pronoun || pronoun == "*") &&
          (d == declension.toUpperCase || declension == "*") &&
          (g == gender.toUpperCase || gender == "*") =>
          (p, g, d, v)
      }
    }
  }.map(data => {
    val counts = Seq(
      data.map(_._1).toSet.size,
      data.map(_._2).toSet.size,
      data.map(_._3).toSet.size,
      Int.MaxValue
    )
    val listValues = data.map(x => List(x._1, x._2, x._3, x._4).zipWithIndex)
    val ordered = listValues.map(_.sortBy(x => counts(x._2)).map(_._1))
    ordered.map(x => (x(0), x(1), x(2), x(3)))
  }).
    map(print3dimension).flatMap(sendBack)

  def print3dimension(data: Seq[(String, String, String, String)]) = {
    data.groupBy(_._1).
      mapValues { _.map {case (_, column, row, value) => (column, row, value) } }.
      mapValues { table =>
        val labelWidth = table.map(_._2.length).max
        val columnWidth = table.map(_._3.length).max
        table.groupBy(_._2).
          mapValues { _.map {case (_, row, value) => (row, value) } }.
          map { case (column, row) =>
            val rowData = row.map { case (_, value) =>
              value.padTo(columnWidth, " ").mkString("")
            }
            (column.padTo(labelWidth, " ").mkString("") +: rowData).
              mkString(s"| ", " | ", " |")
          }.mkString("\n")
      }.map {
        case (title, table) => s"*$title*\n```\n$table\n```"
      }.mkString("\n\n")

  }
//
//  def print3dimension(data: Seq[(String, String, String, String)], xAxis: Seq[String], yAxis: Seq[String], zAxis: Seq[String]) = {
//    zAxis.map { zVal =>
//      zVal -> data.filter(_._1 == zVal).map{ case (_, yVal, xVal, value) => (yVal, xVal, value) }
//    }.filterNot(_._2.isEmpty).map { case (zVal, table) =>
//      val labelWidth = table.map(_._1.length).max
//      val columnWidth = xAxis.map(xVal => xVal -> table.filter(_._2 == xVal)).filterNot(_._2.isEmpty).map(x => x._1 -> x._2.map(_._3.length).max).toMap
//      val tableText = yAxis.map { yVal =>
//        val row = table.filter(_._1 == yVal) map { case (_, xVal, value) => (xVal, value) }
//        val rowLabel = yVal.padTo(labelWidth, " ")
//        val rowValues = xAxis.map(xVal => row.filter(_._1 == xVal).mkString(", ").padTo(columnWidth(xVal), " "))
//        (rowLabel ++ rowValues).mkString(s"| ", " | ", " |")
//      }.mkString("\n")
//      s"*$zVal*\n```\n$tableText\n```"
//    }.mkString("\n\n")
//
//  }

}