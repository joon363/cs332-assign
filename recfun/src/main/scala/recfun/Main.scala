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

    println("balance test")
    print(balance("(if (zero? x) max (/ 1 x))".toList) + " should be true\n")
    print(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList)+ " should be true\n")
    print(balance(":-)".toList)+ " should be false\n")
    print(balance("())(".toList)+ " should be false\n")
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    //edge case
    if(c==0 || c==r) 1
    // recursion
    else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def check(x: Int, auxList: List[Char]): Boolean = {
      if (x < 0) false
      else if (auxList.isEmpty) x == 0
      else {
        check(count(x, auxList.head), auxList.tail)
      }
    }
    def count(num: Int, element: Char): Int = {
      if (element == '(') num + 1
      else if (element == ')') num - 1
      else num
    }

    check(0, chars)
  }

    /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
