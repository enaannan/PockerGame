package com.evolution.bootcamp.assignment.poker

//represents a card
case class Card(suit: Suit, rank: Rank)

object Card {
  val empty: Card = Card(EmptySuit, EmptyRank)
}