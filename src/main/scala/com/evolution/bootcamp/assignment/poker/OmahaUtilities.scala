package com.evolution.bootcamp.assignment.poker

object OmahaUtilities {

  //returns the best hand value based on the Omaha Hold'em specific criteria
  def ohama(handValueCards: List[List[Card]], boardCards: List[Card], handCards: List[Card]): List[Card] = {
    val possibleOmahaValues = handValueCards.map {
      case cards if cards.length == 5 =>
        val quantityFromBoard = cards.count { c => boardCards.contains(c) }
        val quantityFromHand = cards.count { c => handCards.contains(c) }
        if (quantityFromBoard == 3 && quantityFromHand == 2) cards
        else Nil
      case cards => throw new Error(s"can't Evaluate Omaha cards are ${cards}")
    }.filter(_.nonEmpty)
    possibleOmahaValues.head
  }

  //5 cards of the same suit  and return best Omaha option
  def flush(boardCards: List[Card], handCards: List[Card]): List[Card] = {
    val allCards = boardCards ++ handCards
    val flushCards: Map[Suit, List[Card]] = allCards.groupBy(_.suit).filter(group => group._2.length > 4)

    if (flushCards.nonEmpty) {
      val flushCardsOfFive: List[List[Card]] = flushCards.values.flatMap(group => group.combinations(5)).toList
      val sortedFlushCards: List[List[Card]] = flushCardsOfFive.sortBy { cardList => cardList.map(_.rank.value).sum }.reverse
      ohama(sortedFlushCards, boardCards, handCards)

    } else Nil
  }

  //todo: findOmahaHighestHandValue by going through omaha specific methods from top to bottom
  def findOmahaHighestHandValue(hand: List[Card], board: List[Card]): (HandValue, List[Card]) = {
    (Flush, flush(board, hand))
  }
}
