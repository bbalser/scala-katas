package bowling

object Bowling {

  object IsStrike {
    def unapply(pins: List[Int]) = pins match {
      case 10::_ => true
      case _ => false
    }
  }

  object IsSpare {
    def unapply(pins: List[Int]) = pins match {
      case a::b::_ if (a+b == 10) => true
      case _ => false
    }
  }

  def score(line: String): Int = {

    def scoreNextFrame(rolls: List[Int], frame: Int): Int = {
      rolls match {
        case Nil => 0
        case _ if (frame > 10) => 0
        case IsStrike() => rolls.take(3).sum + scoreNextFrame(rolls.drop(1), frame + 1)
        case IsSpare() => rolls.take(3).sum + scoreNextFrame(rolls.drop(2), frame + 1)
        case _ => rolls.take(2).sum + scoreNextFrame(rolls.drop(2), frame + 1)
      }
    }

    val pins = mapToValues(line)
    scoreNextFrame(pins.toList, 1)
  }

  def mapToValues(line: String): Seq[Int] = {
    line.zipWithIndex.map { case (nextChar, index) =>
      nextChar match {
        case 'X' => 10
        case '-' => 0
        case '/' => 10 - line(index-1).getNumericValue
        case x => x.getNumericValue
      }
    }
  }

}
