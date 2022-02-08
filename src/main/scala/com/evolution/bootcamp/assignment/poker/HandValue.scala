package com.evolution.bootcamp.assignment.poker

// represents hand values
sealed abstract class HandValue(val value: Int)

case object StraightFlush extends HandValue(9)

case object FourOfAKind extends HandValue(8)

case object FullHouse extends HandValue(7)

case object Flush extends HandValue(6)

case object Straight extends HandValue(5)

case object ThreeOfAKind extends HandValue(4)

case object TwoPairs extends HandValue(3)

case object Pair extends HandValue(2)

case object HighCard extends HandValue(1)
