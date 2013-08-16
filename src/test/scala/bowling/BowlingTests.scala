package bowling

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

class BowlingTests extends FunSuite with ShouldMatchers {

  test("Bowling") {
    Bowling.score("X") should be (10)
    Bowling.score("XX") should be (30)
    Bowling.score("XX2") should be (36)
    Bowling.score("5/5") should be (20)
    Bowling.score("2-") should be (2)
    Bowling.score("XXXXXXXXXXXX") should be (300)
    Bowling.score("9-9-9-9-9-9-9-9-9-9-") should be (90)
    Bowling.score("5/5/5/5/5/5/5/5/5/5/5") should be (150)
  }

}
