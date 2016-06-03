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
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) {
        return 1
      } else {
        return pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def checkbalance(total: Int, inputchars: List[Char]): Int = {
        if(total < 0) {
          return total
        }else{
          if(inputchars.isEmpty){
            return total
          }else{
            if(inputchars.head == '('){
              checkbalance(total + 1, inputchars.tail)
            }else if (inputchars.head == ')') {
              checkbalance(total - 1, inputchars.tail)
            }else{
              checkbalance(total, inputchars.tail)
            }
          }
        }
      }

      if (checkbalance(0, chars) == 0) {
        return true
      } else {
        return false
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty){
        return 0
      }
      if (money == 0){
        return 1
      }
      return countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
