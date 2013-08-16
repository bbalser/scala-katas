package urinal

class Bathroom(numberOfUrinals: Int, busy: List[Int] = Nil) {

  def findSpot: Int = {
    (numberOfUrinals to 1 by -1).foldLeft((0,0)) { case ((position, bestScore), next) =>
      val score = calculateScore(next)

      if (score > bestScore) {
        (next, score)
      } else {
        (position, bestScore)
      }

    }._1
  }

  def calculateScore(position: Int): Int = {
    var score = 0
    if (!isBusy(position)) {
      if (!isBusy(position+1)) score += 1
      if (!isBusy(position-1)) score += 1
    }
    score
  }

  def isBusy(position: Int): Boolean = {
    busy.find(_ == position) match {
      case Some(x) => true
      case None => false
    }
  }

}
