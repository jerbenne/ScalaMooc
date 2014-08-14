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

  /**
   * Exercise 1
   * Write a function that computes the elements of Pascal’s triangle 
   * by means of a recursive process.
   */
  def pascal(c: Int, r: Int): Int = 
    if (c < 0 || c > r) 0
    else if (c == 0 || c == r) 1
    else pascal (c -1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   * Write a recursive function which verifies the balancing of parentheses in a string
   */
  def balance(chars: List[Char]): Boolean = {
    
    def checkEvenParen(list: List[Char], rightCount: Int): Boolean = {
      if (rightCount < 0) false
      else if (list.isEmpty) true
      else {
        val char = list.head
        if (char == '(') checkEvenParen(list.tail, rightCount + 1)
        else if (char == ')') checkEvenParen(list.tail, rightCount - 1)
        else checkEvenParen(list.tail, rightCount) 
      }
    }
    
    checkEvenParen(chars, 0)
  }
  
  /**
   * Exercise 3
   * Write a recursive function that counts how many different ways you can make change for an amount, 
   * given a list of coin denominations. For example, there are 3 ways to give change for 4 if you have coins 
   * with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
  
	  def sum(list: List[Int], accum: Int, target: Int): Int = {
	    if (accum > target) 0
	    else if (accum == target) 1
	    else {
	      var totalSum = 0
	      var CoinSet = list
	      
	      //The idea is to generate all the unique lists of coin sums to get to the
	      //target total. To accomplish this, we make lists where once a sequence of a number
	      //is finished, that number can no longer be used again.
	      //e.g., 1 + 1 + 2 + 3 is fine, but 1 + 2 + 1 + 3 is not since 1 comes after the 2.
	      //So add the first number of the set, and then recusively add the remaining subset.
	      while (!CoinSet.isEmpty) {
	        totalSum = totalSum + sum(CoinSet, CoinSet.head + accum, target)
	        CoinSet = CoinSet.tail
	      }
	      totalSum
	    }
	  }
  
  sum(coins, 0, money)
  }
}
