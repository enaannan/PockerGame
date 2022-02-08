package com.evolution.bootcamp.assignment.poker

import com.evolution.bootcamp.assignment.poker.Utilities._

object Solver {
  // TODO: implement correct solution logic

  def process(line: String): String = {
    val ErrorPrefix = "Error: "

    line.split("\\s+").toList match {
      case "texas-holdem" :: board :: hands =>
        val boardCard: List[Card] = initBoard(board)
        val handsCard: List[List[Card]] = initHands(hands)
        val players: List[Player] = handsCard map { hand =>
          val (handValue, handValueCards) = findHighestHandValue(hand, boardCard)
          Player(handValue, handValueCards, hand, boardCard)
        }
        val sortedHands: String = sortHands(players)
        sortedHands.trim.replaceAll("\\s{2}", " ")

      case "omaha-holdem" :: board :: hands => ErrorPrefix + "The solution doesn't support Omaha Hold'em"
      case "five-card-draw" :: hands => ErrorPrefix + "The solution doesn't support Five Card Draw"
      case x :: _ => ErrorPrefix + "Unrecognized game type"
      case _ => ErrorPrefix + "Invalid input"
    }
  }
}


