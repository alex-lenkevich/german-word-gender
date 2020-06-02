package link.german.gender.trainer2

import java.util.UUID

import link.german.gender.trainer2.model._
import link.german.gender.trainer2.test._
import link.german.gender.trainer2.ui.{TextTest, Window}
import org.apache.commons.text.similarity.LevenshteinDetailedDistance

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.swing._
import scala.swing.event.{EditDone, Key, KeyPressed, KeyTyped, ValueChanged}

object ui {

  trait Handler {
    def setQuestion(text: String)

    def setHint(text: String)

    def setInput(text: String)

    def setAnswer(answer: Answer)

    def playSound(text: String)
  }


  object Window extends MainFrame {

    font = Font(Font.Monospaced, Font.Style.Plain, 18)
    title = "GUI Program #1"
    preferredSize = new Dimension(1000, 500)

  }

  trait TestPanel {
    val panel: BoxPanel

    def getAnswer: Answer
  }

  class TextTest(test: WordTestResults) extends TestPanel {

    val attempts: mutable.Buffer[String] = mutable.Buffer[String]()
    var gaveUp = false
    var startTime: Long = System.currentTimeMillis()
    val answerPromise: Promise[Answer] = Promise[Answer]

    val questionPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 24)
      background = new Color(0xffffdd)
      editable = false
      text = test.question
      focusable = false
    }

    val hintPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 18)
      background = new Color(0xffffdd)
      editable = false
      text = ""
      focusable = false
    }

    val inputPanel: TextField = new TextField() {
      font = Font(Font.Monospaced, Font.Style.Plain, 24)
    }

    val panel: BoxPanel = new BoxPanel(Orientation.Vertical) {
      contents += questionPanel
      contents += hintPanel
      contents += inputPanel

      listenTo(inputPanel)

      reactions += {
        case EditDone(`inputPanel`) =>
          val correct = test.answer
          val answer = inputPanel.text
          val results = LevenshteinDetailedDistance.getDefaultInstance.apply(answer, correct)
          println(s"answer = $answer")
          println(s"results = $results")
          println(s"attempts = $attempts")
          if (answer == "?") {
            gaveUp = true
            hintPanel.text = correct
            inputPanel.text = ""
          } else if (results.getDistance == 0) {
            answerPromise.success(Answer(!gaveUp, TestMethod.Text, attempts, System.currentTimeMillis() - startTime))
            deafTo(inputPanel)
          } else if (answer == correct.replaceAll("(die|das|der) ", "")) {
            hintPanel.text = "Mit article"
          } else if(!gaveUp) {
            hintPanel.text = s"# Nein (-${results.getDeleteCount} +${results.getInsertCount} ≠${results.getSubstituteCount})!"
            attempts += answer
          }
        case ValueChanged(`inputPanel`) if inputPanel.text != "" =>
          hintPanel.text = ""
        case a =>
          println(a)
          println(s"I focus: ${inputPanel.hasFocus}")
          println(s"Q focus: ${questionPanel.hasFocus}")
          println(s"H focus: ${hintPanel.hasFocus}")
          inputPanel.requestFocus
          inputPanel.requestFocusInWindow()
      }
    }

    override def getAnswer: Answer = Await.result(answerPromise.future, 1.hours)

  }

}


object StartWindow extends App {
  private val test = new TextTest(WordTestResults(
    WordData(UUID.randomUUID().toString, "Buch", "Книга", Some(Kasus.N)),
    TestType.Translate,
    Seq()
  ))
  Window.contents = test.panel
  Window.visible = true
  println(test.getAnswer)
  Window.dispose()
}
