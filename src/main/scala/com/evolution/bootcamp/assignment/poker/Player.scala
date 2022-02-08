package com.evolution.bootcamp.assignment.poker

case class Player(handValue: HandValue, // the handValue a player has
                  handValueCards: List[Card], // the list of cards responsible for the handValue
                  hand: List[Card], // hand of a player
                  boardCards: List[Card] // board cards
                 ) {
  override def toString: String = hand.map {
    card => s"${card.rank.symbol}${card.suit.abbreviation}"
  }.mkString("")

  val remainingCards: List[Card] = {
    (boardCards ++ hand).diff(handValueCards)
  }

val handValueCardsRanksAsString: String = {
  val p: List[Int] = handValueCards.map(_.rank.value)
  p map {
    case 14 => "a"
    case 13 => "b"
    case 12 => "c"
    case 11 => "d"
    case 10 => "e"
    case 9 => "f"
    case 8 => "g"
    case 7 => "h"
    case 6 => "i"
    case 5 => "j"
    case 4 => "k"
    case 3 => "l"
    case 2 => "m"
    case 1 => "n"
    case 0 => "o"
  }
}.mkString
}