package poker

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{GivenWhenThen, FunSpec}

class PokerTests extends FunSpec with ShouldMatchers with GivenWhenThen {

  describe("When dealer evaluates a hand") {

    it("should evaluate high card to 10") {
      Given("A Hand of TH, 9S, 8C, 5D, 2H")
      val hand = List("TH","9S","8C","5D","2H")

      When("dealer evaluates hand")
      val score = Dealer.evaluateHand(hand)

      Then("score is 110")
      score should be (110)
    }

    it("should evaluate high card to J") {
      Given("A Hand of 8H, 9D, 6C, JS, TD")
      val hand = List("8H", "9D", "6C", "JS", "TD")

      When("dealer evaluates hand")
      val score = Dealer.evaluateHand(hand)

      Then("score is 111")
      score should be (111)
    }

    it("should evaluate pair of 8s as 208") {
      val hand = List("8C","4S","3H","8H","QS")
      val score = Dealer.evaluateHand(hand)
      score should be (208)
    }

    it("should evaluate pair of Ks as 213") {
      val hand = List("KH", "8D", "7C", "KD", "3S")
      val score = Dealer.evaluateHand(hand)
      score should be (213)
    }

    it("should evaluate 2 pair of 4s and 5s as 305") {
      val hand = List("AH","4H","5C","5D","4S")
      val score = Dealer.evaluateHand(hand)
      score should be (305)
    }

    it("should evaluate 3 of a kind as 409") {
      val hand = List("9C","8D","9H","5C","9D")
      val score = Dealer.evaluateHand(hand)
      score should be (409)
    }

    it("should evaluate straight to 507") {
      val hand = List("7H","3C","5D","6D","4S")
      val score = Dealer.evaluateHand(hand)
      score should be (507)
    }

    it("should evaluate flushes to 613") {
      val hand = List("2H","5H","9H","QH","KH")
      val score = Dealer.evaluateHand(hand)
      score should be (613)
    }

    it("should evalute full house to 710") {
      val hand = List("4C","4S","TH","TD","TS")
      val score = Dealer.evaluateHand(hand)
      score should be (710)
    }

    it("should evaluate 4 of a kind as 812") {
      val hand = List("QC","QH","QS","QD","2D")
      val score = Dealer.evaluateHand(hand)
      score should be (812)
    }

    it("should evaluate straight flush as 914") {
      val hand = List("AS","KS","QS","JS","TS")
      val score = Dealer.evaluateHand(hand)
      score should be (914)
    }

  }

  describe("When dealer determines a winner") {

    it("should choose highest hand, winner has hight card") {
      val hands = List(List("2H","3D","5S","9C","KD"),List("2C","3H","4S","8C","AH"))

      val (handIndex,score) = Dealer.determineWinner(hands)

      handIndex should be (1)
      score should be (114)
    }

    it("should choose highesthand, winner has flush") {
//      Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S
      val hands = List(List("2H","4S","4C","2D","4H"), List("2S","8S","AS","QS","3S"))
    }

  }

}
