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

    println("count test")
    print(countChange(4,List(1,2))+ " should be 3\n")
    print(countChange(2,List(1,2))+ " should be 2\n")
    print(countChange(2,List(1,3))+ " should be 1\n")
    print(countChange(2,List(5,3))+ " should be 0\n")
    print(countChange(0,List(1,2))+ " should be 1\n")
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
      // more ')': unbalanced
      if (x < 0) false
      // at the end of the list, return true when balanced
      else if (auxList.isEmpty) x == 0
      // recursion without first element
      else {
        check(count(x, auxList.head), auxList.tail)
      }
    }

    // if '(', add one
    // if ')', sub one
    // therefore it can check whether the string is balanced
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
  def countChange(money: Int, coins: List[Int]): Int = {
    // sort coins list in descending order
    val sortedCoins = coins.sorted.reverse
    // return 0 if no more coins available
    if(sortedCoins.isEmpty && money!=0) 0
    // return 1 if money is zero
    else if(money==0) 1
    // exclude coin which has bigger value than remaining money
    else if (sortedCoins.head > money) countChange(money, sortedCoins.tail)
    // first way: use biggest coin, recursion
    // second way: do not use biggest coin, recursion without it
    else countChange(money - sortedCoins.head, sortedCoins) +
         countChange(money, sortedCoins.tail)
  }
}
