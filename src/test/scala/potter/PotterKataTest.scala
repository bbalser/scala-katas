package potter

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import scala.annotation.tailrec

class PotterKataTest extends FunSuite with ShouldMatchers {

  val COST = 8

  test("Should work for a single book") {
    priceOfList(List(1)) should be(8)
  }

  test("should give price for two of the same book") {
    priceOfList(List(1, 1)) should be(16)
  }

  test("should give price for two different books") {
    priceOfList(List(1, 2)) should be(16 * 0.95)
  }
  test("should give price for three different books") {
    priceOfList(List(1, 2, 3)) should be(24 * 0.90)
  }
  test("should give price for three of the same book") {
    priceOfList(List(3, 3, 3)) should be(24)
  }
  test("should give price for four different books") {
    priceOfList(List(1, 2, 3, 4)) should be(32 * 0.80)
  }
  test("should only apply discount to a set of the books") {
    priceOfList(List(1, 1, 2)) should be(16 * 0.95 + 8)
  }

  test("should evaluate more than one unique set") {
    priceOfList(List(1, 1, 2, 2, 3, 3)) should be(24 * 0.90 + 24 * 0.90)
  }

  test("should find lowest possible cost") {
    priceOfList(List(1, 1, 2, 2, 3, 3, 4, 5)) should be(32 * 0.80 + 32 * 0.80)
  }

  def priceOfList(books: List[Int]): Double = {
    def order = createOrder(books)
    totalPriceOfOrder(order)
  }

  private def createOrder(books : List[Int]) : List[Set[Int]] = {

    @tailrec def processBooks(books : List[Int], order : List[Set[Int]]) : List[Set[Int]] = {
      books match {
        case Nil => order
        case book :: tail => {

          findPossibleSets(order, book) match {
            case Nil => processBooks(tail, order :+ Set(book) )
            case possibleSets => processBooks(tail, findLowestPriceOrder(order, possibleSets, book))
          }

        }
      }
    }

    processBooks(books, Nil)
  }

  private def findLowestPriceOrder(order: List[Set[Int]], possibleSets: List[(Set[Int],Int)], book: Int) : List[Set[Int]] = {
    val (newSet, index, price) = possibleSets.map
    { case (possibility, index) =>

      val newPossibility = possibility + book
      (newPossibility, index, totalPriceOfOrder(order.patch(index, List(newPossibility), 1)))

    }.minBy(x => x._3)

    order.patch(index, List(newSet), 1)
  }

  private def findPossibleSets(order : List[Set[Int]], book : Int) : List[(Set[Int], Int)] = order.zipWithIndex.filter { case (set, index) => !set.contains(book) }

  private def totalPriceOfOrder(order: List[Set[Int]]): Double = {
    order.foldLeft(0.0) { (soFar, set) =>
        soFar + priceOfUniqueSet(set)
    }
  }

  private def priceOfUniqueSet(uniqueList: Set[Int]): Double = {
    uniqueList.size * COST * percentOfPrice(uniqueList.size)
  }

  val percentOfPrice = Map(
    1 -> 1.00,
    2 -> 0.95,
    3 -> 0.90,
    4 -> 0.80,
    5 -> 0.75
  )
}
