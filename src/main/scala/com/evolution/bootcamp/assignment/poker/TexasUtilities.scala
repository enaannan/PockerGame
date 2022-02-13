package com.evolution.bootcamp.assignment.poker

object TexasUtilities {
  //finds the sum of a list of cards by their rank value
  def cardsValue(cards: List[Card]): Int = {
    cards.map(_.rank.value).sum
  }

  //a Straight which is also a Flush. Find if hand together with board forms a straightFlush and return best option
  def straightFlush(allCards: List[Card]): List[Card] = {
    val cardsStraight: List[Card] = straight(allCards)
    val cardsFlush: List[Card] = flush(cardsStraight)
    if (cardsFlush.nonEmpty) cardsFlush
    else Nil
  }

  //Four cards of the same rank and return best option
  def fourOfAKind(allCards: List[Card]): List[Card] = {
    val groupedCards: Map[Int, List[Card]] = allCards.groupBy(card => card.rank.value)
    val fourOfAKind: Map[Int, List[Card]] = groupedCards.filter(cards => cards._2.length > 3)
    if (fourOfAKind.nonEmpty) fourOfAKind.values.toList.maxBy(group => group.map(_.rank.value).sum)
    else Nil
  }

  //combination of Three of a kind and a Pair and return best option
  def fullHouse(allCards: List[Card]): List[Card] = {
    val threeOfAKindList = threeOfAKind(allCards)
    if (threeOfAKindList.nonEmpty) {
      val remCards = allCards.diff(threeOfAKindList)
      val pairList = pair(remCards)
      if (pairList.nonEmpty) threeOfAKindList ++ pairList
      else Nil
    } else Nil
  }

  //5 cards of the same suit  and return best option
  def flush(allCards: List[Card]): List[Card] = {
    val flushCards: Map[Suit, List[Card]] = allCards.groupBy(_.suit).filter(group => group._2.length > 4)

    if (flushCards.nonEmpty) {
      val flushCardsOfFive: List[List[Card]] = flushCards.values.flatMap(group => group.sliding(5, 1)).toList
      val bestFlush = flushCardsOfFive.maxBy { cardList => cardList.map(_.rank.value).sum }
      bestFlush
    } else Nil
  }

  //sequence of 5 cards of consecutive rank (note an exception - A can both precede 2 and follow K) and return best option
  def straight(allCards: List[Card]): List[Card] = {
    def consecutiveSeq(cards: List[List[Card]]): List[List[Card]] = {
      cards.filter { group =>
        val indexedGroup = group.zipWithIndex
        indexedGroup.forall {
          case (card, i) if i < indexedGroup.length - 1 => card.rank.value + 1 == indexedGroup(i + 1)._1.rank.value
          case _ => true
        }
      }
    }

    val seqOfCards = allCards.sortBy(_.rank.value).distinct
    if (seqOfCards.length < 5) Nil
    else {
      val ace = seqOfCards.find(_.rank == Ace)
      if (ace.isEmpty) {
        val groupsOfFive = seqOfCards.sliding(5, 1).toList
        val consecutiveList = consecutiveSeq(groupsOfFive)
        if (consecutiveList.nonEmpty) consecutiveList.maxBy(group => group.map(_.rank.value).sum)
        else Nil
      }
      else {
        val listWithAceAsMax = seqOfCards.sliding(5, 1).toList
        val listWithAceAsMin = (ace.get.copy(rank = AceSpecial) +: seqOfCards.reverse.tail.reverse).sliding(5, 1).toList
        val consecutiveList = consecutiveSeq(listWithAceAsMin ++ listWithAceAsMax)
        if (consecutiveList.nonEmpty) consecutiveList.maxBy(group => group.map(_.rank.value).sum)
        else Nil
      }
    }
  }

  //three cards with the same rank  and return best option
  def threeOfAKind(allCards: List[Card]): List[Card] = {
    val groupedCards = allCards.groupBy(card => card.rank.value)
    val threeOfAKind = groupedCards.filter(cards => cards._2.length > 2)
    if (threeOfAKind.nonEmpty) threeOfAKind.values.toList.maxBy(group => group.map(_.rank.value).sum)
    else Nil
  }

  //two Pair-s  and return best option
  def twoPair(allCards: List[Card]): List[Card] = {
    val firstPair = pair(allCards)
    val secondPair = pair(allCards.diff(firstPair))
    if ((firstPair ++ secondPair).length == 4) firstPair ++ secondPair
    else Nil
  }

  //  two cards of the same rank  and return best option
  def pair(allCards: List[Card]): List[Card] = {
    val groupedCards = allCards.groupBy(card => card.rank.value)
    val pair = groupedCards.filter(cards => cards._2.length > 1)
    if (pair.nonEmpty) pair.values.toList.maxBy(group => group.map(_.rank.value).sum)
    else Nil
  }

  //the "fallback" in case no other hand value rule applies
  def highCard(allCards: List[Card]): List[Card] = {
    allCards.sortBy(_.rank.value).take(5).reverse
  }

  //return type is (handValue eg Flush, actual cards eg Ks8s7sAs9s)
  def findTexasHighestHandValue(hand: List[Card], board: List[Card]): (HandValue, List[Card]) = {
    val allCards: List[Card] = hand ++ board

    if (straightFlush(allCards).nonEmpty) (StraightFlush, straightFlush(allCards))
    else if (fourOfAKind(allCards).nonEmpty) (FourOfAKind, fourOfAKind(allCards))
    else if (fullHouse(allCards).nonEmpty) (FullHouse, fullHouse(allCards))
    else if (flush(allCards).nonEmpty) (Flush, flush(allCards))
    else if (straight(allCards).nonEmpty) (Straight, straight(allCards))
    else if (threeOfAKind(allCards).nonEmpty) (ThreeOfAKind, threeOfAKind(allCards))
    else if (twoPair(allCards).nonEmpty) (TwoPairs, twoPair(allCards))
    else if (pair(allCards).nonEmpty) (Pair, pair(allCards))
    else (HighCard, highCard(allCards))
  }

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

  def sortByHandValue(players: List[Player]): Map[Int, List[Player]] = {
    players.groupBy(_.handValue.value).toList.sortBy(_._1).toMap
  }

  def sortByThreeOfAKind(players: List[Player]): Map[Int, List[Player]] = {
    players.groupBy(player => cardsValue(player.handValueCards.take(3))).toList.sortBy(_._1).toMap
  }

  def sortByPair(players: List[Player]): Map[Int, List[Player]] = {
    players.groupBy(player => cardsValue(player.handValueCards.reverse.take(2))).toList.sortBy(_._1).toMap
  }

  //remainder of cards ie boardCards - handValueCards
  def sortByRemainingCards(players: List[Player]): Map[Int, List[Player]] = {
    players.groupBy(player => cardsValue(player.remainingCards)).toList.sortBy(_._1).toMap
  }

  def sortByHandValueCardsRankSum(players: List[Player]): Map[Int, List[Player]] = {
    players.groupBy(player => cardsValue(player.handValueCards)).toList.sortBy(_._1).toMap
  }

  def sortRanks(ranks: List[List[Rank]], pos: Int): Map[Int, List[List[Rank]]] = {
    ranks.groupBy { rank => rank(pos).value }.toList.sortBy(_._1).toMap
  }

  def sortByHighCard(players: List[Player]): Map[String, List[Player]] = {
    val groupedPlayers: Map[String, List[Player]] = players.groupBy { player => player.handValueCardsRanksAsString } //grouped players with same HighCards
    val ranks: List[String] = groupedPlayers.keySet.toList // these ranks are distinct
    val sorted = ranks.sorted.reverse
    Map(sorted map {
      st => (st -> groupedPlayers(st))
    }: _*)
  }

  def sortHands(players: List[Player]): String = {

    def equalHands(players: List[Player]): String = {
      val playerList = players map (_.toString)
      playerList.sorted.mkString("=")
    }

    def breakTiesWithRemCards(players: List[Player]): String = {
      val sortedByRemOfHandValueCards = sortByRemainingCards(players)
      val res = sortedByRemOfHandValueCards map {
        case (_, players) if players.length > 1 => equalHands(players)
        case (_, player) => s" ${player.head.toString()} "
      }
      res.mkString
    }

    def breakFullHouseTiesSecondLevel(players: List[Player]): String = {
      val sortedByPair = sortByPair(players)
      val res = sortedByPair map {
        case (_, players) if players.length > 1 => breakTiesWithRemCards(players)
        case (_, player) => s" ${player.head.toString()} "
      }
      res.mkString
    }

    def breakFullHouseTies(players: List[Player]): String = {
      val sortedByThreeOfAKind = sortByThreeOfAKind(players)
      val res = sortedByThreeOfAKind map {
        case (_, players) if players.length > 1 => breakFullHouseTiesSecondLevel(players)
        case (_, player) => s" ${player.head.toString()} "
      }
      res.mkString
    }

    def breakHighCardTies(players: List[Player]): String = {
      val sortedByHighCard: Map[String, List[Player]] = sortByHighCard(players)
      val res = sortedByHighCard map {
        case (_, players) if players.length > 1 => breakTiesWithRemCards(players)
        case (_, player) => s" ${player.head.toString()} "
      }
      res.mkString
      //      sortedByHighCard.mkString
    }

    def breakTies(players: List[Player]): String = {
      val sortedByHandValueRankSum = sortByHandValueCardsRankSum(players)
      val res = sortedByHandValueRankSum map {
        case (_, players) if players.length > 1 => breakTiesWithRemCards(players)
        case (_, player) => s" ${player.head.toString()} "
      }
      res.mkString
    }

    val initialSort: Map[Int, List[Player]] = sortByHandValue(players)

    val res = initialSort map {
      case (_, players) if players.length > 1 && players.head.handValue == FullHouse => breakFullHouseTies(players)
      case (_, players) if players.length > 1 && players.head.handValue == HighCard => breakHighCardTies(players)
      case (_, players) if players.length > 1 => breakTies(players)
      case (_, player) => s" ${player.head.toString()} "
    }
    res.mkString
  }
}
