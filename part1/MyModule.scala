
object MyModule {
  def abs(x: Int): Int = 
    if(x < 0) -x
    else x

  def fib(n: Int): Int = {
    def go(n: Int): Int =
      if(n == 0) 0
      else if (n == 1) 1
      else go(n - 1) + go(n - 2)

    go(n)
  }

  def fibonnaci(n: Int): Int = {
    def loop(n: Int, prev: Int, curr: Int): Int = 
      if(n == 0) prev
      else loop(n - 1, curr, prev + curr)
    loop(n, 0, 1)    
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int =
      if(n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

 def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if(n >= as.length) -1
      else if(p(as(n))) n
      else loop(n + 1)
    loop(0)
  }
  
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if(n >= as.length - 1) true
      else if(ordered(as(n), as(n + 1))) false
      else go(n + 1)
    go(0)
  }

  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }  
  
  def main(args: Array[String]): Unit = 
    // println(formatResult("absolute value", -100, abs))    
    // println(formatResult("factorial", 10, factorial))
}
