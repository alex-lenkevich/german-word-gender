package link.german.gender.service

import scala.concurrent.{ExecutionContext, Future}

class DeclensionService extends Service {

  val Declensions = Seq("nominativ", "akkusativ", "dativ", "genitiv")
  val Genders = Seq("maskulin", "feminin", "neutrum", "plural")
  val Regex = "(\\(\\)|\\s)*(->|'| (to|in) | )(\\(\\)|\\s)*"

  val PersonalPronoun2: Seq[(String, String, String)] = Seq(
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

  val PersonalPronoun3: Seq[(String, String, String, String)] = Seq(

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

  val Order = Declensions ++ Genders ++ PersonalPronoun3.map(_._1)


  override def applicable(msg: String): Boolean = msg.matches(".*" + Regex + ".*")

  override def process[T](msg: String, sendBack: String => Future[T])(implicit ec: ExecutionContext): Future[T] = Future {
    msg.trim.split(Regex).toList match {
      case pronoun :: declension :: Nil =>
        PersonalPronoun2.collect {
          case (p, d, v) if
            (p == pronoun || pronoun == "*") &&
            (d.substring(0, 1) == declension.toLowerCase.substring(0, 1) || declension == "*") =>
            ("", p, d, v)
        }
      case pronoun :: gender :: declension :: Nil =>
        PersonalPronoun3.collect {
        case (p, d, g, v) if
          (p == pronoun || pronoun == "*") &&
          (d.substring(0, 1) == declension.toLowerCase.substring(0, 1) || declension == "*") &&
          (g.substring(0, 1) == gender.toLowerCase.substring(0, 1) || gender == "*") =>
          (p, g, d, v)
      }
    }
  }.map(data => {
    val counts = Seq(
      data.map(_._1).toSet.size,
      data.map(_._2).toSet.size,
      data.map(_._3).toSet.size,
      Int.MinValue + 2
    ).zipWithIndex.map {
      case (1, i) => Int.MaxValue - i
      case x => x._1
    }

    val listValues = data.map(x => List(x._1, x._2, x._3, x._4).zipWithIndex)
    val ordered = listValues.map(_.sortBy(x => -counts(x._2)).map(_._1))
    ordered.map(x => (x(0), x(2), x(1), x(3)))
  }).
    map(print3dimension).
    flatMap(sendBack)

  def print3dimension(data: Seq[(String, String, String, String)]) = {
    val tableLabels: Seq[String] = data.map(_._1).distinct.sortBy(Order.indexOf)
    val rowLabels: Seq[String] = data.map(_._2).distinct.sortBy(Order.indexOf)
    val columnLabels: Seq[String] = data.map(_._3).distinct.sortBy(Order.indexOf)

    tableLabels.map {tableLabel =>
      val table = data.collect { case (`tableLabel`, row, column, value) => (row, column, value) }

      val labelColumnWidth = table.map(_._1.length).max
      val columns = table.groupBy(_._2)
      val columnWidth = columns.map { case (columnLabel, column) =>
        columnLabel -> (columnLabel.length +: column.map(_._3.length)).max
      }

      val headRow = ("".padTo(labelColumnWidth, " ").mkString("") +: columnLabels.map(x => x.padTo(columnWidth(x), " ").
        mkString(""))).
        mkString(s"| ", " | ", " |")

      val tableBody = rowLabels.map { rowLabel =>
        val rowData = columnLabels.map { columnLabel =>
          table.collect { case (`rowLabel`, `columnLabel`, value) =>
            value.padTo(columnWidth(columnLabel), " ").mkString("")
          }.head
        }
        (rowLabel.padTo(labelColumnWidth, " ").mkString("").mkString("") +: rowData).mkString(s"| ", " | ", " |")
      }
      tableLabel -> (headRow +: tableBody).mkString("\n")
    }.map {
      case (title, table) => s"*$title*\n```\n$table\n```"
    }.mkString("\n\n")
  }

}