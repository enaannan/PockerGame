package com.evolution.bootcamp.assignment.poker

object Solver {
  // TODO: implement correct solution logic

  //a Straight which is also a Flush
  def straightFlush(hand: String, board: String): Option[String] = ???

  //Four cards of the same rank
  def fourOfAKind(hand: String, board: String): Option[String] = ???

  //combination of Three of a kind and a Pair
  def fullHouse(hand: String, board: String): Option[String] = ???

  //5 cards of the same suit
  def flush(hand: String, board: String): Option[String] = ???

  //sequence of 5 cards of consecutive rank (note an exception - A can both precede 2 and follow K)
  def straight(hand: String, board: String): Option[String] = ???

  //three cards with the same rank
  def threeOfAKind(hand: String, board: String): Option[String] = ???

  //two Pair-s
  def twoPair(hand: String, board: String): Option[String] = ???

  //two cards of the same rank
  def pair(hand: String, board: String): Option[String] = ???

  //the "fallback" in case no other hand value rule applies
  def highCard(hand: String, board: String): Option[String] = ???


  //return type is (handValue eg Flush, actual cards eg Ks8s7sAs9s)
  def findHighestHandValue(hand: String, board: String): (String, String) = ???

  //todo: sorting hands should take care of hands with the same value
  def sortHands(highestHandValues: List[(String, String)]): List[String] = ???


  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands =>
        val highestHandValues: List[(String, String)] = hands map {
          findHighestHandValue(_, board)
        }
        sortHands(highestHandValues) mkString (" ")
      case "omaha-holdem" :: board :: hands => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => ErrorPrefix + "The solution doesn't support Five Card Draw"
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }
}
