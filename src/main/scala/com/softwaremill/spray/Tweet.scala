package com.softwaremill.spray

import org.json4s.native.Serialization
import org.json4s.native.Serialization._
import org.json4s.ShortTypeHints
import scala.util.Random

trait Tweet

case class PostedTweet(user: Int, content: String) extends Tweet

object Tweet {

  val tweets = List[Tweet]()

  private implicit val formats = Serialization.formats(ShortTypeHints(List(classOf[PostedTweet])))
  def toJson(tweets: List[Tweet]): String = writePretty(tweets)
  def toJson(tweet: Tweet): String = writePretty(tweet)
}