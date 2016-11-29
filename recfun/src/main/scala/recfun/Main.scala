package recfun

import scala.annotation.tailrec

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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def check(acc: Int, split: List[Char]): Boolean =
      if (acc < 0) false
      else if (split.isEmpty) true
      else {
        if (split.head == '(')
          check(acc + 1, split.tail)
        else if (split.head == ')')
          check(acc - 1, split.tail)
        else
          check(acc, split.tail)
      }
    check(0, chars)
  }

    def countChange(money: Int, coins: List[Int]): Int = {
      if(money == 0) 1
      else if (!coins.isEmpty && money > 0) countChange(money - coins.head, coins) + countChange(money, coins.tail)
      else 0
    }
  }
