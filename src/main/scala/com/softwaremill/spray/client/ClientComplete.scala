package com.softwaremill.spray.client

import spray.http._
import akka.actor._
import scala.collection.mutable.ArrayBuffer
import spray.client.pipelining._
import akka.actor.ActorSystem
import scala.util.Random
import scala.concurrent.duration._

case class tweet(i: Int)
case class START(system: ActorSystem)
case object STOP

object ClientComplete extends App {

  implicit val system = ActorSystem("Tweeter")
  val master = system.actorOf(Props[Master], name="master")
  
  master ! START(system)
}  
  
class Master extends Actor {
  
	def receive = {
	
		case START(system) =>
			val usercount = 5
			val cores: Int = Runtime.getRuntime().availableProcessors()
    		val twitteruser = new ArrayBuffer[ActorRef]()
			for(i<-0 to (cores-1))
			{
				twitteruser += context.actorOf(Props[TwitterUser], name = "twitteruser"+i)
			}
			import system.dispatcher
			for(i<-0 to (usercount-1))
				system.scheduler.schedule(0 milliseconds, 10 milliseconds, twitteruser(i%cores), tweet(i))

	}
}
			
class TwitterUser extends Actor {
		
		def receive = {
		
			case tweet(i) =>
				import scala.concurrent.ExecutionContext.Implicits.global
				val pipeline = sendReceive
				var str = Random.alphanumeric.take(10).mkString
				if(Random.nextInt(2) == 0)
					str = str+"hashtag"+Random.nextInt(10)
				pipeline(Post("http://localhost:8080/tweet/new?user="+i+"&content="+str))
				
		}
}
