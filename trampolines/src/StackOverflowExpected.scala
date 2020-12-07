object StackOverflowExpected {
  case class State[S, +A](runS : S =>(A, S)) {
    def map[B](f: A => B): State[S, B] =
      State[S, B](s => {
        val(a, s1) = runS(s)
       (f(a), s1)
      })
      
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B](s => {
        val(a, s1) = runS(s)
        f(a) runS s1
      })
  }

  def getState[S]: State[S, S] = State(s =>(s,s))
  
  def setState[S](s: S): State[S, Unit] = State(_ =>((),s))
  
  def pureState[S, A](a: A): State[S, A] = State(s =>(a,s))

  def zipIndex[A](as: List[() => A]): List[(Int,A)] =
    as.foldLeft(
      pureState[Int, List[(Int,A)]](List()) // the machine outputs List() on every transition(which are all identities)
   )((acc, a) => { // acc is state machine(starting with pure state above; a is an element of the list
        for {
          xs <- acc // output of state machine, of type List[(Int,A)]]
          n  <- getState // output is int, because that's how we want it in line 30
          _  <- setState(n + 1)
        } yield(n,a())::xs
      }
   ).runS(0)._1.reverse

  def even[A](ns: List[() => A]): Boolean =
    ns match {
      case Nil => true
      case x :: xs => x(); odd(xs)
    }
    
  def odd[A](ns : List[() => A]): Boolean =
    ns match {
      case Nil => false
      case x :: xs => x(); even(xs)
    }
  
  def main(args: Array[String]): Unit = {

    val crashingList: List[() => Int] =(0 to 20) map(n =>() => {
      if(n > 10) throw new IllegalArgumentException("oops"); n*n
    }) toList

    try {
      val indexed = zipIndex(crashingList)
    } catch {
      case exex: IllegalArgumentException =>
        val stacktrace = exex.getStackTrace
        val mainEntry = stacktrace(6)
        for {
          i <- 7 to 14
        } require(stacktrace(i) == mainEntry, s"Ex.1. We expected nine identical stacktrace rows, failed at $i")
    }
    println("Stacktrace demonstrates that we will have a stack overflow on large collections in zipIndex.")

    try {
      val whoCares = even(crashingList)
    } catch {
      case exex: IllegalArgumentException =>
        val stacktrace = exex.getStackTrace
        val mainEntry = stacktrace(2)
        for { i <- 4 to 12 by 2 } {
          require(stacktrace(i) == mainEntry, s"Ex.2. We expected nine identical rows, failed at $i\n${stacktrace mkString "\n"}")
        }
    }
    println("Stacktrace demonstrates that we will have a stack overflow on large collections in mutual recursion.")
  }
}
