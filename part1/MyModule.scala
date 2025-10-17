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
  //
  // alternative implementation
  //
  // def fibonnaci(n: Int): Int = {
  //   def loop(n: Int, prev: Int, curr: Int): Int = 
  //     if(n == 0) 0
  //     else loop(n - 1, curr, curr + prev)
  //
  //   loop(n, 0, 1)
  // }

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
      if(n == as.length - 1) true
      else if(!(ordered(as(n), as(n + 1)))) false
      else go(n + 1)
    go(0)
  }

  def ordered(a: Int, b: Int): Boolean = a <= b 
  
  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }  

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }
  
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = 
    println(formatResult("absolute value", -100, abs))    
    println(formatResult("factorial", 4, factorial))

    // Test cases for isSorted function
    val a = Array(1, 2, 3, 4, 5)
    val b = Array(1, 0, 3, 4, 5)

    assert((isSorted(a, (x: Int, y: Int) => x <= y) == true))
    assert((isSorted(b, (x: Int, y: Int) => x <= y) == false))

    // Test cases for compose function
    val f = (x: Int) => x + 2
    val g = (y: Int) => y - 2

    assert((compose(f, g)(3) == 3))
    assert((compose(f, g)(-1) == -1))
}
