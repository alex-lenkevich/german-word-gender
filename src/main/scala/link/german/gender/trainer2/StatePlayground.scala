package link.german.gender.trainer2

import link.german.gender.trainer2.Main.loadWords
import link.german.gender.trainer2.test.TestType.Present3
import zio._
import zio.console._

object StatePlayground extends App with StateService {


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      words <- loadWords()
      state <- loadState(words)
      diff <- Task {
        state.states.filter(x => Present3.isSupported(x.data))
          .map(s => (s.data.de -> s.data.present3) -> (
            s.data.de.replaceAll(" \\(sich\\)", "")
              .replaceAll("([td])e?n$", "$1et")
              .replaceAll("e?n$", "t") ->
              s.data.present3.get
                .replaceAll(" sich", "")
                .split(" ").reverse.mkString
            )).toMap
          .mapValues {
            case (de, p3) =>
              val (de_, p3_) = de.zipAll(p3, ' ', ' ').dropWhile(x => x._1 == x._2).unzip
              de_.mkString.trim -> p3_.mkString.trim
          }
          .mapValues {
            case (de, p3) =>
              val (de_, p3_) = de.reverse.zipAll(p3.reverse, ' ', ' ').dropWhile(x => x._1 == x._2).unzip
              de_.reverse.mkString.trim -> p3_.reverse.mkString.trim
          }
          .filter {
            case (_, ("", "")) => false
            case _ => true
          }
          .mapValues(x => s"${x._1} => ${x._2}")
      }
      _ <- putStrLn(diff.map { case (de, diff) => s"$de: $diff" }.mkString("\n"))
    } yield 0
    }.orDie


}
