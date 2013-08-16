package urinal

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

class UrinalTests extends FunSuite with ShouldMatchers {

  test("Use farthest when all urinals are empty") {
    val bathroom = new Bathroom(2)
    bathroom.findSpot should be (2)
  }

  test("Always use farthest from door") {
    val bathroom = new Bathroom(14)
    bathroom.findSpot should be (14)
  }

  test("Do not stand next to another dude") {
    val bathroom = new Bathroom(10,List(10))
    bathroom.findSpot should be (8)
  }

  test("Do not stand next to another dude when multiple are busy") {
    val bathroom = new Bathroom(10, List(10, 8))
    bathroom.findSpot should be (6)
  }

  test("2 dudes, one broke a rule") {
    val bathroom = new Bathroom(10, List(10, 9))
    bathroom.findSpot should be (7)
  }

  test("busy bathroom, and have to stand next to somebody") {
    val bathroom = new Bathroom(10, List(10, 8, 5, 2))
    bathroom.findSpot should be (7)
  }

  test("busy bathroom, and have to stand closest to door") {
    val bathroom = new Bathroom(10, List(10, 8, 6, 4, 2))
    bathroom.findSpot should be (1)
  }

  test("should choose urinal closest 2 door, if mean avoiding standing next to dude") {
    val bathroom = new Bathroom(10, List(10, 8, 6, 4, 3))
    bathroom.findSpot should be (1)
  }

}
