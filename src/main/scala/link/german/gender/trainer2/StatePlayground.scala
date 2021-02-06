package link.german.gender.trainer2

import link.german.gender.client.DudenClient
import zio._

object StatePlayground extends App with StateService {


  override def run(args: List[String]): ZIO[zio.ZEnv, Nothing, Int] = {
    for {
      words <- loadWords()
      wordsWithSynonyme <- ZIO.collectAllPar(
        words.map(
          w => w.synonyme
            .fold(
              DudenClient.dudenRequestSynonym(w.de)
                .map(Some(_))
                .catchAll(_ => UIO(None))
            )(
              x => UIO(Some(x))
            )
            .map(s => w.copy(synonyme = s))))
      _ <- saveWords(wordsWithSynonyme)
    } yield 0
  }.orDie


}
