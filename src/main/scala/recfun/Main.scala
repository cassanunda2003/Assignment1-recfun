package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(column: Int, row: Int): Int = {
    if(column == 0 || row == 0 || column == row)
    {
      return 1
    }
    else
    {
      pascal(column - 1, row - 1) + pascal(column,row - 1)
    }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def accumulator(bracketsCount: Int, bracket: List[Char]) : Int = {
      bracket.headOption match {
        case None => bracketsCount
        case Some('(') => accumulator(bracketsCount + 1, bracket.tail)
        case Some(')') => if (bracketsCount <= 0) return 1 else accumulator(bracketsCount - 1, bracket.tail)
        case Some(_) => accumulator(bracketsCount, bracket.tail)
      }
    }
    accumulator(0, chars) == 0
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

    def helper(denoms: List[Int], amt: Int): Int = {

      if(amt < 0) return 0
      if(amt == 0) return 1

      if(denoms.isEmpty && amt >= 1) return 0

    //  println(denoms.toString + " " + count +" "+ amt)
      helper(denoms.init, amt) + helper(denoms, amt - denoms.last)
    }
    helper(coins, money)
  }
  }
