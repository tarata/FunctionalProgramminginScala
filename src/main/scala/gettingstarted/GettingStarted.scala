package gettingstarted

object MyModule {
  def abs(n:Int):Int = if(n < 0) -n else n

  private def formatAbs(x :Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main (args: Array[String]) {
    println(formatAbs(-1))
  }

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
      if(n <= 1) acc1 + acc2
      else go(n-1, acc2, acc2 + acc1)

    go(n, 0, 1)
  }
}
