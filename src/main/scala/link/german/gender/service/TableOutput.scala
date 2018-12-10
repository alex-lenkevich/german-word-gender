package link.german.gender.service

trait TableOutput {

  val Order: Option[Seq[String]] = None

  def print3dimension(data: Seq[(String, String, String, String)]) = {
    val (tableLabels: Seq[String], rowLabels: Seq[String], columnLabels: Seq[String]) = {
      Order.fold {(
        data.map(_._1).distinct,
        data.map(_._2).distinct,
        data.map(_._3).distinct
      )}{ order => (
        data.map(_._1).distinct.sortBy(order.indexOf),
        data.map(_._2).distinct.sortBy(order.indexOf),
        data.map(_._3).distinct.sortBy(order.indexOf)
      )}
    }

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
          table.collectFirst {
            case (`rowLabel`, `columnLabel`, value) => value
          }.getOrElse("-||-").padTo(columnWidth(columnLabel), " ").mkString("")
        }
        (rowLabel.padTo(labelColumnWidth, " ").mkString("").mkString("") +: rowData).mkString(s"| ", " | ", " |")
      }
      tableLabel -> (headRow +: tableBody).mkString("\n")
    }.map {
      case (title, table) => s"*$title*\n```\n$table\n```"
    }.mkString("\n\n")
  }



}
