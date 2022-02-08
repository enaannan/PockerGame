package com.evolution.bootcamp.assignment.poker

// represents Suits
sealed abstract class Suit(val abbreviation: String)

case object Heart extends Suit("h")

case object Diamond extends Suit("d")

case object Spade extends Suit("s")

case object Club extends Suit("c")

case object Empty extends Suit("e")

case object EmptySuit extends Suit("empty")