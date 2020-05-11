package link.german.gender.trainer

import java.awt.Toolkit
import java.awt.datatransfer.DataFlavor
import java.io.FileWriter
import java.util.logging.{Level, Logger}

import link.german.gender.PonsClient
import org.jnativehook.{GlobalScreen, NativeInputEvent}
import org.jnativehook.keyboard.{NativeKeyEvent, NativeKeyListener}
import link.german.gender.SyntaxSugar._
import sys.process._

import scala.util.Try

object GlobalKeyListener extends NativeKeyListener {

  private val writer = new FileWriter("unknown_words.csv", true)

  Logger.getLogger(classOf[GlobalScreen].getPackage.getName).setLevel(Level.SEVERE)

  private val cmd: Int = NativeInputEvent.META_L_MASK
  private val option: Int = NativeInputEvent.ALT_L_MASK
  private val shift: Int = NativeInputEvent.SHIFT_L_MASK

  override def nativeKeyTyped(nativeKeyEvent: NativeKeyEvent): Unit = {

  }

  def isHotKey(nativeKeyEvent: NativeKeyEvent) = {
    val mod = nativeKeyEvent.getModifiers
    (mod & cmd) != 0 && (mod & shift) != 0 && nativeKeyEvent.getKeyCode == 46
  }

  override def nativeKeyPressed(nativeKeyEvent: NativeKeyEvent): Unit = {
    if(isHotKey(nativeKeyEvent)) {
      Try(
        Toolkit.getDefaultToolkit.getSystemClipboard.getContents(this).getTransferData(DataFlavor.stringFlavor)
      )
        .map(_.toString.trim.replaceAll("^[^a-zA-ZäüöÄÜÖß]+|[^a-zA-ZäüöÄÜÖß]+$", ""))
        .tap(_.foreach { word =>
          writer.append(s"$word\n")
          writer.flush()
          println(word)
          val text = PonsClient.readFromPons(word)
            .zipWithIndex
            .map(x => s"${x._2}, ${x._1.translate}".replaceAll("\n", " ")).mkString("\\n")
          println(text)
          Runtime.getRuntime.exec(Array[String]("osascript", "-e",
            s""" display notification "$text" with title "Deutsch Trainer" subtitle "$word" sound name "Funk" """ )
          )
        })
    }
  }

  override def nativeKeyReleased(nativeKeyEvent: NativeKeyEvent): Unit = {

  }
}
