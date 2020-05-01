package link.german.gender.trainer

import java.io.{File, FileInputStream, FileWriter}
import java.nio.file.Files
import java.time.LocalDateTime
import java.util.concurrent.{Executor, Executors}

import io.circe.parser._
import io.circe.syntax._
import javazoom.jl.player.Player
import link.german.gender.PonsAudioDownloader
import link.german.gender.SyntaxSugar._
import link.german.gender.trainer.enums.AnswerType._
import link.german.gender.trainer.enums.MemoryState
import link.german.gender.trainer.enums.MemoryState.MemoryState
import link.german.gender.trainer.model._
import org.apache.commons.text.similarity.{LevenshteinDetailedDistance, LevenshteinDistance}

import scala.collection.Seq
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor, Future}
import scala.io.{BufferedSource, Source, StdIn}
import scala.util.{Random, Try}

object Main extends App with PonsAudioDownloader {

  private val executorService = Executors.newSingleThreadExecutor()
  implicit val soundEc: ExecutionContextExecutor = ExecutionContext.fromExecutor(executorService)

  sys.addShutdownHook(
    () => executorService.shutdown()
  )

  def loadState: Seq[WordState] = {
    val source: BufferedSource = Source.fromFile("trainer_state.json")
    val state = for {
      json <- source.getLines().mkString =>> parse
      state <- json.as[Seq[WordState]]
    } yield state
    source.close()
    val time = LocalDateTime.now.plusWeeks(1)
    state.map(_
      .filterNot(_.state == MemoryState.Persistent)
      .filter(_.nextAskDate.exists(_.isBefore(time)))
      .sortBy(_.nextAskDate)
      .foreach(x => println(x.toString)))
    state
  }.fold(e => throw new Exception("Can't load state from file", e), x => x)

  def saveState(state: Seq[WordState]): Seq[WordState] = {
    val writer = new FileWriter("trainer_state.json")
    state.asJson.noSpaces =>> writer.write
    writer.close()
    state
  }

  def learnNew(state: Seq[WordState]): Seq[WordState] = {
    val newState = try {
      val states = state
        .filter(_.word.genus.exists(_.contains("f")))
        .filter(_.shouldBeAsked).sortBy(
        x => (x.nextAskDate.getOrElse(LocalDateTime.MAX),
          x.word.genus,
          x.word.name.toLowerCase.replaceAll("^(.+ )?(be|ge|ent|emp|er|ver|miß|zer|los|an|ab|bei|mit|ein|vor|nach|uber|über|zu|unter|um|aus|auf)+", "$1"),
        )
      )
      states.take(15) =>> learn
    } catch {
      case ExitException(ns) => throw ExitException(state.diff(ns) ++ ns)
    }
    newState.map(x => s"${x.word.name}: ${x.state} (${x.answers})")
    (state.diff(newState) ++ newState)
  }

  Try {
    Files.copy(new File("trainer_state.json").toPath, {
      val file = new File(s"backup/trainer_state_${System.currentTimeMillis()}.json")
      file.toPath
    })
    (1 to 1000).foldLeft(loadState){
      (state, _) =>
        println(state.groupBy(_.state).mapValues(_.length).map{
          case (k, v) => s"$k: $v"
        }.mkString("State: ", ", ", "\n"))
        state =>> learnNew =>> saveState
    }
  }.recover {
    case ExitException(s) => s
  }.foreach(saveState)

  def readAnswer() = {
    print("> ")
    StdIn.readLine()
  }

  def learn(words: Seq[WordState]): Seq[WordState] = {
    println(words.map(_.toString).mkString("\n"))
    (words.filter(_.shouldBeAsked) =>> Random.shuffle) match {
      case Seq() => words
      case word +: _ =>
        System.out.print("\033[H\033[2J");
        System.out.flush();
        println(s"# ${word.word.translate}")
        val startTime = System.currentTimeMillis()
        @scala.annotation.tailrec
        def getAnswer(attempt: Int): Answer = {
          val answer = readAnswer()
          val correct = word.word.name
          if(answer == "exit") {
            throw ExitException(words)
          } else if(answer == "") {
            printAnswerWithSound(word.word)
            while(readAnswer != correct){}
            Answer(Incorrect, attempt, System.currentTimeMillis() - startTime)
          } else {
            val results = LevenshteinDetailedDistance.getDefaultInstance.apply(answer, correct)
            if(results.getDistance == 0) {
              playSound(word.word)
              Answer(Correct, attempt, System.currentTimeMillis() - startTime)
            } else if(answer == correct.replaceAll("(die|das|der) ", "")) {
              "# Mit dem Artikel!" =>> println
              getAnswer(attempt)
            } else {
              s"# Nein (-${results.getDeleteCount} +${results.getInsertCount} ≠${results.getSubstituteCount})!" =>> println
              getAnswer(attempt + 1)
            }
          }
        }

        val state = word.withAnswer(getAnswer(1))

        (words.filterNot(_==state) :+ state) =>> learn
    }

  }

  def playSound(word: Word): Unit = Future {
    val filePath = getPonsAudio(word.name)
    val player = new Player(new FileInputStream(filePath))
    player.play()
    player.close()
  }

  def printAnswerWithSound(word: Word): Unit = {
    s"# ${word.name}" =>> println
    playSound(word)
  }

  case class ExitException(state: Seq[WordState]) extends Exception

}


