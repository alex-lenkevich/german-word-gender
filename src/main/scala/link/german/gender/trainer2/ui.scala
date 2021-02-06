package link.german.gender.trainer2

import link.german.gender.client.DudenClient

import java.util.UUID
import javax.swing.border.EmptyBorder
import link.german.gender.trainer2.model._
import link.german.gender.trainer2.test._
import link.german.gender.trainer2.ui.{SelectTest, TextTest, Window}
import org.apache.commons.text.similarity.LevenshteinDetailedDistance
import zio.UIO

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, Promise}
import scala.swing._
import scala.swing.event.{EditDone, Key, KeyPressed, ValueChanged}

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
    foreground = new Color(0xFF222222)
    background = new Color(0xFF222222)

  }

  trait TestPanel {
    val panel: BoxPanel

    def getAnswer: Answer
  }

  class TextTest(test: WordTestResults, progress: (Int, Int)) extends TestPanel {

    val attempts: mutable.Buffer[String] = mutable.Buffer[String]()
    var gaveUp = false
    var startTime: Long = System.currentTimeMillis()
    val answerPromise: Promise[Answer] = Promise[Answer]

    val progressbar: ProgressBar = new ProgressBar {
      value = progress._1
      max = progress._2
      background = new Color(0xFF222222)
      foreground = new Color(0xFF222222)
      focusable = false
    }

    val questionPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 48)
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF222222)
      editable = false
      text = test.question
      focusable = false
    }

    val synonymePanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 24)
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF222222)
      editable = false
      text = test.data.synonyme.getOrElse("No synonyms")
      focusable = false
    }

    val hintPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 48)
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF222222)
      editable = false
      text = ""
      focusable = false
    }

    val inputPanel: TextField = new TextField() {
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF333333)
      font = Font(Font.Monospaced, Font.Style.Plain, 48)
      border = new EmptyBorder(0, 0, 0, 0)
    }

    val panel: BoxPanel = new BoxPanel(Orientation.Vertical) {
      contents += progressbar
      contents += questionPanel
      contents += synonymePanel
      contents += hintPanel
      contents += inputPanel

      listenTo(inputPanel)

      reactions += {
        case EditDone(`inputPanel`) =>
          val correct = test.answer
          val answer = inputPanel.text
          val results = LevenshteinDetailedDistance.getDefaultInstance.apply(answer, correct)
          if (answer == "") {
            // ignore
          } else if (attempts.contains(answer)) {
            hintPanel.text = s"Nein distance = ${results.getDistance}!"
          } else if (answer == "?") {
            gaveUp = true
            hintPanel.text = correct
            inputPanel.text = ""
          } else if (results.getDistance == 0) {
            answerPromise.success(Answer(!gaveUp, TestMethod.Text, attempts, System.currentTimeMillis() - startTime))
            deafTo(inputPanel)
          } else if (answer == correct.replaceAll("(die|das|der) ", "")) {
            hintPanel.text = "Mit article"
          } else if (!gaveUp) {
//            hintPanel.text = s"# Nein (-${results.getDeleteCount} +${results.getInsertCount} ≠${results.getSubstituteCount})!"
            hintPanel.text = s"Nein distance = ${results.getDistance}!"
            attempts += answer
          } else {
            hintPanel.text = correct
          }
        case ValueChanged(`inputPanel`) if inputPanel.text != "" =>
          hintPanel.text = ""
        case a =>
          inputPanel.requestFocus
          inputPanel.requestFocusInWindow()
      }
    }

    override def getAnswer: Answer = Await.result(answerPromise.future, 100.hours)

  }

  class SelectTest(test: WordTestResults, options: Seq[String], progress: (Int, Int)) extends TestPanel {

    val startTime: Long = System.currentTimeMillis()
    val answerPromise: Promise[Answer] = Promise[Answer]
    var wrong: Boolean = false

    val progressbar: ProgressBar = new ProgressBar {
      value = progress._1
      max = progress._2
      background = new Color(0xFF222222)
      foreground = new Color(0xFF222222)
      focusable = false
    }

    val questionPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 48)
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF222222)
      editable = false
      text = test.question
      focusable = false
    }

    val hintPanel: TextPane = new TextPane() {
      font = Font(Font.Monospaced, Font.Style.Plain, 48)
      foreground = new Color(0xFFFF0000)
      background = new Color(0xFF222222)
      editable = false
      text = ""
      focusable = false
    }

    val selectListView: ListView[String] = new ListView(options) {
      font = Font(Font.Monospaced, Font.Style.Plain, 36)
      foreground = new Color(0xFFDEDEDE)
      background = new Color(0xFF333333)
      border = new EmptyBorder(2, 2, 2, 2)

      this.selectIndices(1)
    }

    val panel: BoxPanel = new BoxPanel(Orientation.Vertical) {

      contents += progressbar
      contents += questionPanel
      contents += hintPanel
      contents += new ScrollPane(selectListView)

      listenTo(selectListView.keys)

      reactions += {
        case KeyPressed(`selectListView`, Key.Enter, _, _) =>
          if(wrong) {
            answerPromise.success(Answer(correct = false, TestMethod.Select, Seq(), System.currentTimeMillis() - startTime))
          } else if(selectListView.selection.items.head == test.answer)  {
            answerPromise.success(Answer(correct = true, TestMethod.Select, Seq(), System.currentTimeMillis() - startTime))
          } else {
            hintPanel.text = test.answer
            wrong = true
          }
        case _ =>
          selectListView.requestFocus
          selectListView.requestFocusInWindow()
      }

    }

    override def getAnswer: Answer = {
      selectListView.requestFocus
      selectListView.requestFocusInWindow()
      Await.result(answerPromise.future, 1.hours)
    }

  }

}


object StartWindow extends App {
  private val test = new TextTest(WordTestResults(
    WordData(UUID.randomUUID().toString, "Buch", "Книга", kasus = Some(Kasus.N)),
    TestType.Translate,
    Seq()
  ), (4 -> 10))
  Window.contents = test.panel
  Window.visible = true
  Window.dispose()
}

object StartSelectWindow extends App {
  private val word = WordTestResults(
    WordData(UUID.randomUUID().toString, "Buch", "Книга", kasus = Some(Kasus.N)),
    TestType.KasusTestType,
    Seq()
  )
  private val test = new SelectTest(word, TestType.KasusTestType.options(word.data, Seq(word.data), 6), (4 -> 10))
  Window.contents = test.panel
  Window.visible = true
  Window.dispose()
}
