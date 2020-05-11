package link.german.gender.trainer

import link.german.gender.{LingvoClient, ReversoClient}
import zio._
import zio.console._

object StateFixer extends App with StateService with LingvoClient {
  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      state <- loadState
      newState <- Task {
        state ++ state.collect {
          case x if x.word.name.startsWith("die ") && x.word.genus.contains("m(f)") =>
            x.copy(word = x.word.copy(name = x.word.name.replaceAll("^die ", "der ")))
        }
      }
      _ <- saveState(newState)
    } yield 0
  }.orDie
}
