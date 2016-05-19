package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    for (i <- 0 to args.length - 1)
      println(balance(args(i).toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def map(letter: Char): Int = {
      if (letter == '(') 1
      else if (letter == ')') -1
      else 0
    }
    def helper(chars: List[Char], curt: Int): Boolean = {
      if (curt < 0) false
      else if (chars.isEmpty) curt == 0
      else {
        helper(chars.tail, curt + map(chars.head))
      }
    }
    helper(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def sum(n: Int): Int = {
      var count = 0
      for (i <- 0 to n)
        count += countChange(money - i * coins.head, coins.tail)
      count
    }

    if (coins.isEmpty) 0
    else if (coins.length == 1) {
      if (money % coins.head == 0) 1
      else 0
    } else {
      val n = money / coins.head
      sum(n)
    }
  }
}
