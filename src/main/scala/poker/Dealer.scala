package poker

import scala.annotation.tailrec

object Dealer {

  val STRAIGHT_FLUSH_BASE = 900
  val FOUR_OF_A_KIND_BASE = 800
  val FULL_HOUSE_BASE = 700
  val FLUSH_BASE = 600
  val STRAIGHT_BASE = 500
  val THREE_OF_A_KIND_BASE = 400
  val TWO_PAIR_BASE = 300
  val PAIR_BASE = 200
  val HIGH_CARD_BASE = 100

  def determineWinner(hands: List[List[String]]): (Int,Int) = {
    val scores = hands.map(evaluateHand)
    val highScore = scores.max
    (scores.indexOf(highScore), highScore)
  }


  def evaluateHand(hand: List[String]): Int = {
    hand match {
      case StraightFlush(x) => x
      case FourOfAKind(x) => x
      case FullHouse(x) => x
      case Flush(x) => x
      case Straight(x) => x
      case ThreeOfAKind(x) => x
      case TwoPair(x) => x
      case Pair(x) => x
      case x => highCard(hand)
    }
  }

  object StraightFlush {
    def unapply(hand: List[String]): Option[Int] = {
      (hand, hand) match {
        case (Flush(x), Straight(y)) => Some(STRAIGHT_FLUSH_BASE + numberValueOfHighestCard(hand))
        case _ => None
      }
    }
  }

  object FullHouse {
    def unapply(hand: List[String]): Option[Int] = {
      (hand,hand) match {
        case (Pair(x), ThreeOfAKind(y)) => Some(FULL_HOUSE_BASE+numberValueOfHighestCard(hand))
        case _ => None
      }
    }
  }

  object Flush {
    def unapply(hand: List[String]): Option[Int] = {
      if (hand.map(_(1)).distinct.size == 1) {
        Some(FLUSH_BASE + numberValueOfHighestCard(hand))
      } else {
        None
      }
    }
  }

  object Straight {
    def unapply(hand: List[String]): Option[Int] = {

      @tailrec def checkForStraight(values: List[Int]): Option[Int] = {
        values match {
          case a::b::tail if (a + 1 == b) => checkForStraight(b::tail)
          case a::b::tail if (a + 1 != b) => None
          case a::tail => Some(STRAIGHT_BASE + a)
        }
      }

      val numericValues = hand.map(numberValue).sorted
      checkForStraight(numericValues)
    }
  }

  object FourOfAKind {
    def unapply(hand: List[String]): Option[Int] = findSameNumberCards(hand, 4, 1, FOUR_OF_A_KIND_BASE)
  }

  object ThreeOfAKind {
    def unapply(hand: List[String]): Option[Int] = findSameNumberCards(hand, 3, 1, THREE_OF_A_KIND_BASE)
  }

  object TwoPair {
    def unapply(hand: List[String]): Option[Int] = findSameNumberCards(hand, 2, 2, TWO_PAIR_BASE)
  }

  object Pair {
    def unapply(hand: List[String]): Option[Int] = findSameNumberCards(hand, 2, 1, PAIR_BASE)
  }

  private def findSameNumberCards(hand: List[String], howManyCards: Int, howManySets: Int, baseScore: Int): Option[Int] = {
    val sets = hand.map(_(0)).groupBy(x => x).filter { case (card, cards) => cards.size == howManyCards }
    if (sets.size == howManySets) {
      Some(baseScore + sets.map(x => numberValue(x._1.toString)).max)
    } else {
      None
    }
  }

  private def highCard(hand: List[String]): Int = {
    HIGH_CARD_BASE + numberValueOfHighestCard(hand)
  }

  private def numberValueOfHighestCard(hand: List[String]): Int = {
    hand.map(numberValue).max
  }

  private def numberValue(card: String): Int = {
    card(0) match {
      case 'A' => 14
      case 'K' => 13
      case 'Q' => 12
      case 'J' => 11
      case 'T' => 10
      case x => x.getNumericValue
    }
  }



}
