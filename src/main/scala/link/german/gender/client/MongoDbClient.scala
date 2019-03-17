package link.german.gender.client

import link.german.gender.client.MongoDbClient.WordGender
import link.german.model.Gender
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.bson.codecs.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.result.UpdateResult
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}

import scala.concurrent.Future

class MongoDbClient {

  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[WordGender]), DEFAULT_CODEC_REGISTRY )
  val mongoClient: MongoClient = MongoClient("mongodb://gender:gender2012@ds259253.mlab.com:59253/heroku_pr40hfc1")
  val database: MongoDatabase = mongoClient.getDatabase("heroku_pr40hfc1").withCodecRegistry(codecRegistry)
  val collection: MongoCollection[WordGender] = database.getCollection("gender")

  def save(entity: WordGender): Future[UpdateResult] = {
    collection.replaceOne(Filters.eq("_id", entity._id), entity, ReplaceOptions().upsert(true)).toFuture()
  }

  def load(word: String): Future[Option[WordGender]] = {
    collection.find(Filters.eq("_id", word)).headOption()
  }

}

object MongoDbClient {
  case class WordGender(_id: String, gender: Gender)
}
