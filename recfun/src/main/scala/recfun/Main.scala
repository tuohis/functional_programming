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
  }
  
  def min(xs: List[Int]): Int = try {
      if (xs.head < min(xs.tail)) xs.head else min(xs.tail)
    } catch {
      case e: java.util.NoSuchElementException => xs.head
    }
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if (r < 0 || c < 0 || c > r) throw new java.util.NoSuchElementException else if (r == 0 || c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = innerBalance(chars, 0)
  
  def innerBalance(chars: List[Char], opens: Int): Boolean = if (chars.isEmpty && opens == 0) true
    else if ((chars.isEmpty && opens > 0) || opens < 0) false
  	else if (chars.head == ')') innerBalance(chars.tail, opens-1)
  	else if (chars.head == '(') innerBalance(chars.tail, opens+1) 
  	else innerBalance(chars.tail, opens) 

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = if (money <= 0 || coins.isEmpty) 0 else
    innerCountChange(money, coins, 0)
  
  def innerCountChange(money: Int, coins: List[Int], value: Int): Int = if (coins.isEmpty) 0
  	else if (money == 0) 1 
    else if (money < coins.head) innerCountChange(money, coins.tail, value)
    else innerCountChange(money - coins.head, coins, value) + innerCountChange(money, coins.tail, value+1) 
      
  
}
