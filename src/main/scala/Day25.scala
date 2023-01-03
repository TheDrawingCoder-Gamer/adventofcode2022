object Day25 {
  def snafuToDec(input: String): Long = {
    def helper(input: List[Char], accum: Long): Long = {
      input match {
        case c :: tail => 
          val part = c match {
            case '1' => 1
            case '0' => 0
            case '2' => 2 
            case '-' => -1
            case '=' => -2 
          }
          helper(tail, (accum * 5) + part) 
        case Nil => accum   
      }    
    }
    helper(input.toList, 0)
  }
  def digitStream: LazyList[Int] = {
    // 1, 2, 5, 10, 25, 50 
    LazyList.iterate(1) { it => 
      if ((it % 2) == 0) 
        (it / 2) * 5
      else 
        it * 2
    }
  }
  def toSnigit(digit: Int): Char = {
    digit match {
      case -2 => '='
      case -1 => '-'
      case 0 => '0'
      case 1 => '1'
      case 2 => '2'
      case _ => ???
    }
  }
  /*
  final def decToSnafu(n: Int): String = {
    if (n == 0) return "0"
    // Find smallest value representable with "one digit"
    // that is still bigger than n 
    

    val digit = digitStream.find(_ >= n).get 
    val isTwo = (digit % 2) == 0
    val place = 
      if (isTwo)
        (digit / 2) / 5
      else 
        digit / 5
    val placeValue = place * 5 
    val lowerPlaceValue = (place - 1) * 5 
    if (digit == n) {
      val padding = "0" * (place - 1)
      if (isTwo)
        "2" + padding 
      else 
        "1" + padding 
    // If it is within range to use = or - 
    } else if ((digit - n) <= (lowerPlaceValue * 2)) {
      val remaining = digit - n 
      if (remaining <= lowerPlaceValue)
        remaining
        
    } else {
    
    }
  }
  */
  def decToSnafu(n: Long): String = {
    val sunits = Iterator.unfold(n) { n => 
      Option.when(n != 0) {
        val s = math.floorMod(n + 2, 5).toInt - 2
        toSnigit(s) -> (n - s) / 5 
      }
    }
    if (sunits.isEmpty) "0"
    else sunits.mkString.reverse
  }
  def parse(input: String): List[Long] = {
    input.linesIterator.map(snafuToDec _).toList
  }
  def run(input: String): String = {
    decToSnafu(parse(input).sum)
  }
}
