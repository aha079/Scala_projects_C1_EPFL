package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    // println("Pascal's Triangle")
    // for row <- 0 to 10 do
    //   for col <- 0 to row do
    //     print(s"${pascal(col, row)} ")
    //   println()
    println(countChange(4,List(1,2)))

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = if r==1 || r==0 || c==0 || c==r then 1 else pascal(c,r-1)+pascal(c-1,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =  
    def isbal(num: Int, chars: List[Char]): Boolean =
      if chars.isEmpty then num==0 else
        if chars.head == '(' then isbal(num+1,chars.tail) 
        else if chars.head==')' then 
          if num==0 then false else isbal(num-1,chars.tail) 
        else isbal(num,chars.tail)
    isbal(0,chars)
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    if coins.isEmpty || money<0 then 0 else if money==0 then 1 else countChange(money-coins.head,coins)+countChange(money,coins.tail)
