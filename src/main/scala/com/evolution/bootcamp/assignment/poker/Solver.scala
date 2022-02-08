package com.evolution.bootcamp.assignment.poker

object Solver {
  // TODO: implement correct solution logic

  // represents Suits
  sealed trait Suit

  case object Heart extends Suit

  case object Diamond extends Suit

  case object Spade extends Suit

  case object Club extends Suit

  // represents hand values
  sealed trait HandValues

  case object StraightFlush extends HandValues

  case object FourOfAKind extends HandValues

  case object FullHouse extends HandValues

  case object Flush extends HandValues

  case object Straight extends HandValues

  case object ThreeOfAKind extends HandValues

  case object TwoPairs extends HandValues

  case object Pair extends HandValues

  case object HighCard extends HandValues

  //represents card ranks
  sealed abstract class Rank(val value: Int)

  case object Ace extends Rank(14)

  case object King extends Rank(13)

  case object Queen extends Rank(12)

  case object Jack extends Rank(11)

  case object Ten extends Rank(10)

  case object Nine extends Rank(9)

  case object Eight extends Rank(8)

  case object Seven extends Rank(7)

  case object Six extends Rank(6)

  case object Five extends Rank(5)

  case object Four extends Rank(4)

  case object Three extends Rank(3)

  case object Two extends Rank(2)

  //represents a card
  case class Card(suit: Suit, rank: Rank)

  //a Straight which is also a Flush. Find if hand together with board forms a straightFlush and return best option
  def straightFlush(hand: String, board: String): Option[String] = ???

  //Four cards of the same rank and return best option
  def fourOfAKind(hand: String, board: String): Option[String] = ???

  //combination of Three of a kind and a Pair and return best option
  def fullHouse(hand: String, board: String): Option[String] = ???

  //5 cards of the same suit  and return best option
  def flush(hand: String, board: String): Option[String] = ???

  //sequence of 5 cards of consecutive rank (note an exception - A can both precede 2 and follow K) and return best option
  def straight(hand: String, board: String): Option[String] = ???

  //three cards with the same rank  and return best option
  def threeOfAKind(hand: String, board: String): Option[String] = ???

  //two Pair-s  and return best option
  def twoPair(hand: String, board: String): Option[String] = ???

  //two cards of the same rank  and return best option
  def pair(hand: String, board: String): Option[String] = ???

  //the "fallback" in case no other hand value rule applies
  def highCard(hand: String, board: String): Option[String] = ???


  //return type is (handValue eg Flush, actual cards eg Ks8s7sAs9s)
  def findHighestHandValue(hand: List[Card], board: List[Card]): (String, String) = ???

  //todo: sorting hands should take care of hands with the same value
  def sortHands(highestHandValues: List[(String, String)]): List[String] = ???

  def rank(st: String): Rank = {
    st match {
      case "A" => Ace
      case "K" => King
      case "Q" => Queen
      case "J" => Jack
      case "T" => Ten
      case "9" => Nine
      case "8" => Eight
      case "7" => Seven
      case "6" => Six
      case "5" => Five
      case "4" => Four
      case "3" => Three
      case "2" => Two
      case _ => throw new Error(s"Rank error $st")
    }
  }

  def suit(st: String): Suit = {
    st match {
      case "h" => Heart
      case "d" => Diamond
      case "c" => Club
      case "s" => Spade
      case _ => throw new Error(s"Suit error $st")
    }
  }

  def initBoard(board: String): List[Card] = {
    board.grouped(2).toList.map { card =>
      val (r, s) = card.splitAt(1)
      Card(suit(s), rank(r))
    }
  }

  def initHands(hands: List[String]): List[List[Card]] = {
    hands.map { hand =>
      hand.grouped(2).toList.map { card =>
        val (r, s) = card.splitAt(1)
        Card(suit(s), rank(r))
      }
    }
  }

  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands =>
        val boardCard = initBoard(board)
        val handsCard = initHands(hands)

        val highestHandValues: List[(String, String)] = handsCard map { hand =>
          findHighestHandValue(hand, boardCard)
        }
        sortHands(highestHandValues) mkString (" ")
      case "omaha-holdem" :: board :: hands => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => ErrorPrefix + "The solution doesn't support Five Card Draw"
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }
}
