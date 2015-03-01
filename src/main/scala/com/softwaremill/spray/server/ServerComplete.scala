package com.softwaremill.spray.server

import akka.actor.{Props, Actor, ActorSystem}
import spray.routing._
import com.softwaremill.spray._
import spray.http.MediaTypes
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable._

object ServerComplete extends App with SimpleRoutingApp {

  val usercount = 5
  var tweetcount = 0
  implicit val actorSystem = ActorSystem("Twitter")
  
  val followerlist = new ArrayBuffer[ArrayBuffer[Int]] ()

  for(i<-0 to (usercount-1))
  {
   	  val tempFollow = new ArrayBuffer[Int]         
   	  followerlist+= tempFollow
  }
  
  for(i<-0 to (usercount-1))
  {
	var fol= Random.nextInt(usercount-1)
    while (fol==0)
    	fol=Random.nextInt(usercount-1)
    for(j<- 0 to fol-1)
    {
      var follower_number=Random.nextInt(usercount-1)
      if(!followerlist(i).isEmpty)
		while (followerlist(i).contains(follower_number) || follower_number==i)
			follower_number=Random.nextInt(usercount-1)
	  followerlist(i)+= follower_number 
    }
  }
  //println(followerlist)
  
  var userTweets = new ListBuffer[Queue[String]] ()
  for(i <- 0 to usercount-1)
  {
   	  val Q = new Queue[String]
   	  userTweets+= Q
  }
  var hashtagTweets = new ListBuffer[Queue[String]] ()
  for(i <- 0 to 9)
  {
   	  val Qu = new Queue[String]
   	  hashtagTweets+= Qu
  }
  
  var favourTweets = new ListBuffer[Queue[String]] ()
  for(i <- 0 to 9)
  {
   	  val Que = new Queue[String]
   	  favourTweets+= Que
  }
  
  var allTweets = Tweet.tweets
  
  def getJson(route: Route) = get {
    respondWithMediaType(MediaTypes.`application/json`) { route }
  }

  startServer(interface = "localhost", port = 8080) {
    get {
      path("") { ctx =>
        ctx.complete("Welcome to Tweeter!")
      }
    } ~
    getJson {
      path("list" / "all") {
        complete {
          Tweet.toJson(allTweets)
        }
      }
    } ~
   	post {
      path("tweet" / "new") {
        parameters("user".as[Int], "content".as[String]) { (user, content) =>
		  tweetcount = tweetcount + 1
          val newTweet = PostedTweet(user, content)
          allTweets = newTweet :: allTweets												////// NEW TWEET //////
		  var randstr = content
		  if(randstr.contains("hashtag"))												////// HASHTAG //////
		  {
		    var htag = randstr.split("hashtag")
			var htagnum = htag(1).toInt
			if(hashtagTweets(htagnum).length >= 5)
				hashtagTweets(htagnum).dequeue
			hashtagTweets(htagnum) += user+":"+content
		  }
		  if(userTweets(user).length >= 5)												////// FOLLOWER LIST //////
			userTweets(user).dequeue
		  userTweets(user) += content
		  val k = followerlist(user).length
		  for(i<-0 to k-1)
		  {
		    if(userTweets(followerlist(user)(i)).length >= 5)
				userTweets(followerlist(user)(i)).dequeue
			userTweets(followerlist(user)(i)) += user+":"+content
		  }
          complete {
            "OK"
          }
        }
      }
    }
  }
  
  val flag = 0
  while(flag == 0)
  {
    println("Welcome! Please Enter your username")
	val p = Console.readInt()
	println("Your 5 recent tweets:")													////// RECENT TWEETS //////
	println(userTweets(p))
	println("Do you want to follow anyone?(Y/N)")										////// FOLLOW SOMEONE //////
	val yesno1 = Console.readLine()
	if(yesno1.toLowerCase() == "y")
	{
		println("Whom do you want to follow?")
		val following = Console.readInt()
		if(following == p)
			println("You cannot follow yourself")
		else if(!followerlist(following).contains(p))
			println("You already follow him")
		else
			followerlist(following) += p
	}
	println("Do you want to unfollow anyone?(Y/N)")										////// UNFOLLOW SOMEONE //////
	val yesno2 = Console.readLine()
	if(yesno2.toLowerCase() == "y")
	{
		println("Whom do you want to unfollow?")
		val unfol = Console.readInt()
		if(unfol == p)
			println("You cannot unfollow yourself")
		else if(!followerlist(unfol).contains(p))
			println("You are not following him!")
		else
		{
			for(i<-0 to (followerlist(unfol).length - 1))
			{
				if(followerlist(unfol)(i) == p)
				{
					var temp = followerlist(unfol)(i)
					followerlist(unfol)(i) = followerlist(unfol)(followerlist(unfol).length - 1)
					followerlist(unfol)(followerlist(unfol).length - 1) = temp
					//followerlist(unfol).dequeue
				}
			}
		}
	}
	println("Which tweet do you want to retweet?")										////// RETWEET //////
	val x = Console.readInt()
	if(userTweets(p)(x).contains(":"))
	{
		if(userTweets(p).length >= 5)
			userTweets(p).dequeue
		userTweets(p) += userTweets(p)(x)
		val k = followerlist(p).length
		for(i<-0 to k-1)
		{
			if(userTweets(followerlist(p)(i)).length >= 5)
			userTweets(followerlist(p)(i)).dequeue
			userTweets(followerlist(p)(i)) += p+":"+userTweets(p)(x)
		}
	}
	else
	{
		println("You cannot retweet your own tweet")
	}
	println("Which tweet is your favourite?")											////// FAVOURITE TWEETS //////
	var d = Console.readInt()
	if(favourTweets(p).length >=5)
		favourTweets(p).dequeue
	favourTweets(p) += userTweets(p)(d)
	println("Your Favourite Tweets:")
	println(favourTweets(p))
	println("Enter a hashtag:")															////// HASHTAG //////
	var m = Console.readInt()
	println("Recent tweets on this hashtag:")
	println(hashtagTweets(m))
	//println("Total number of tweets="+tweetcount)
	println("Press Enter to continue")
	val a = Console.readLine()
  }
}
