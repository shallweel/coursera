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
  def pascal(c: Int, r: Int): Int =
    if (r == 0) 1
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def step(acc: Int, cs: List[Char]): Boolean =
      if (acc < 0) false
      else if (cs.isEmpty) acc == 0
      else {
        val head = cs.head
        val incr = if (head == '(') 1 else if (head == ')') -1 else 0
        step(acc + incr, cs.tail)
      }
    step(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def step(m: Int, cs: List[Int], initial: Boolean): Int =
      if (cs.isEmpty || m < 0) 0
      else if (m == 0) if (initial) 0 else 1
      else if (cs.tail.isEmpty) if (m % cs.head == 0) 1 else 0
      else step(m, cs.tail, initial = false) + step(m - cs.head, cs, initial = false)
    step(money, coins, initial = true)
  }
}
