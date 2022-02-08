package com.evolution.bootcamp.assignment.poker

//Card ranks
sealed abstract class Rank(val value: Int, val symbol: String)

case object Ace extends Rank(14, "A")

case object King extends Rank(13, "K")

case object Queen extends Rank(12, "Q")

case object Jack extends Rank(11, "J")

case object Ten extends Rank(10, "T")

case object Nine extends Rank(9, "9")

case object Eight extends Rank(8, "8")

case object Seven extends Rank(7, "7")

case object Six extends Rank(6, "6")

case object Five extends Rank(5, "5")

case object Four extends Rank(4, "4")

case object Three extends Rank(3, "3")

case object Two extends Rank(2, "2")

case object AceSpecial extends Rank(1, "A")

case object EmptyRank extends Rank(0, "0")
