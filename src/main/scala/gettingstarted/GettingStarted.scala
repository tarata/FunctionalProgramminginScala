package gettingstarted

object MyModule {

  def main (args: Array[String]) {
    println(formatAbs(-1))
    println(formatFactorial(7))
  }

  def abs(n:Int):Int = if(n < 0) -n else n


  def factorial(n:Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc:Int): Int =
      if(n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  def fib(n:Int) : Int = {
    @annotation.tailrec
    def go(n:Int, acc1:Int, acc2:Int): Int =
      if(n <= 0) acc1
      else if(n == 1) acc2
      else go(n-1, acc2, acc1 + acc2)

    go(n, 0, 1)
  }

  private def formatAbs(x :Int) = formatResult("absolute", x, abs)

  private def formatFactorial(n: Int) = formatResult("factorial", n ,factorial)

  def formatResult(name:String, n:Int, f:Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def findFirst(ss: Array[String], key:String): Int = {
    @annotation.tailrec
    def loop(n:Int): Int =
      if(n >= ss.length) -1
      else if ( ss(n) == key) n
      else loop(n+1)

    loop(0)
  }


  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n:Int):Int =
      if(n >= as.length) -1
      else if(p(as(n)))n
      else loop(n+1)

    loop(0)
  }

  def isSorted[A](as:Array[A], ordered: (A,A) => Boolean): Boolean = {

    @annotation.tailrec
    def loop(n:Int):Boolean = {
      if(n >= as.length - 1) true
      else if(ordered(as(n), as(n+1))) loop(n+1)
      else false
    }

    loop(0)
  }
}
