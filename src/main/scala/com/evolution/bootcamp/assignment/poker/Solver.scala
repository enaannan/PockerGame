package com.evolution.bootcamp.assignment.poker

import com.evolution.bootcamp.assignment.poker.TexasUtilities._

object Solver {
  def simulate(board: String, hands: List[String]): String = {
    val boardCard: List[Card] = initBoard(board)
    val handsCard: List[List[Card]] = initHands(hands)
    val players: List[Player] = handsCard map { hand =>
      val (handValue, handValueCards) = findTexasHighestHandValue(hand, boardCard)
      Player(handValue, handValueCards, hand, boardCard)
    }
    val sortedHands: String = sortHands(players)
    sortedHands.trim.replaceAll("\\s{2}", " ")
  }

  def process(line: String): String = {
    val ErrorPrefix = "Error: "
    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands => simulate(board, hands)
      case "omaha-holdem" :: board :: hands => simulate(board, hands)
      case "five-card-draw" :: hands => simulate("", hands)
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }
}


