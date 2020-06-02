package link.german.gender

import java.util

import com.itextpdf.kernel.pdf.canvas.parser.EventType.RENDER_TEXT
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader}
import com.itextpdf.kernel.pdf.canvas.parser.{EventType, PdfCanvasProcessor}
import com.itextpdf.kernel.pdf.canvas.parser.data.{IEventData, TextRenderInfo}
import com.itextpdf.kernel.pdf.canvas.parser.listener.{FilteredEventListener, IEventListener}

import scala.collection.JavaConverters._
import scala.collection.mutable

object PdfParser extends App {

  val SRC = "/Users/oleksandr.linkevych/Downloads/netzwerk_a1-b1_unregelmaessigeverben (1).pdf"

  val pdfDoc = new PdfDocument(new PdfReader(SRC))

  //        pdfDoc.getPage(10)

  //        CustomAlignFilter fontFilter = new CustomAlignFilter();
  val listener = new FilteredEventListener

  // Create a text extraction renderer
  val extractionStrategy = listener.attachEventListener(new PdfEventListener)

  // Note: If you want to re-use the PdfCanvasProcessor, you must call PdfCanvasProcessor.reset()
  val parser = new PdfCanvasProcessor(listener)
  for (i <- 1 to pdfDoc.getNumberOfPages) {
    parser.processPageContent(pdfDoc.getPage(i))
  }

  println(extractionStrategy.getText)

  class PdfEventListener extends IEventListener {

    case class TextBlock(text: String, left: Float, top: Float, right: Float, bottom: Float)

    val dataSeq: mutable.Buffer[TextBlock] = mutable.Buffer[TextBlock]()

    def cluster[T](seq: Seq[T], join: PartialFunction[(T, T), T]): Seq[T] = {
      seq.foldLeft(Seq[T]()){
        case (Seq(), t) => Seq(t)
        case (buf :+ last, t) => buf ++ join.andThen(Seq(_)).applyOrElse(last -> t, (_: (T, T)) => Seq(last, t))
      }
    }

    def getText: String = dataSeq
      .groupBy(_.top)
      .mapValues(v => cluster[TextBlock](v.sortBy(_.left), {
        case (v1, v2) if Math.abs(v2.left - v1.right) < 1 =>
          TextBlock(v1.text + v2.text, v1.left, v1.top, v2.right, Math.max(v1.bottom, v2.bottom))
      })).toSeq.sortBy(_._1).map(_._2).map(_.map(_.text.trim).mkString("|")).mkString("\n")

    override def eventOccurred(data: IEventData, `type`: EventType): Unit = {
      val info = data.asInstanceOf[TextRenderInfo]
      dataSeq += TextBlock(info.getText,
        info.getBaseline.getBoundingRectangle.getLeft,
        info.getBaseline.getBoundingRectangle.getTop,
        info.getBaseline.getBoundingRectangle.getRight,
        info.getBaseline.getBoundingRectangle.getBottom
      )
    }

    override def getSupportedEvents: util.Set[EventType] = Set(RENDER_TEXT).asJava
  }


}
