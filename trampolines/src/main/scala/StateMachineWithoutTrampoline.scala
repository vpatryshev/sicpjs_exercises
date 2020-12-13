object StateMachineWithoutTrampoline extends Testing {

  case class State[S, +A](step : S =>(A, S)) {
    def map[B](f: A => B): State[S, B] = flatMap(a => State[S, B](s => (f(a), s)))

    def altMap[B](f: A => B): State[S, B] =
      State[S, B](s => {
        val(a, s1) = step(s)
        (f(a), s1)
      })
      
    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State[S, B](s => {
        val(a, s1) = step(s)
        f(a) step s1
      })
  }

  def getState[S]: State[S, S] = State(s =>(s,s))
  
  def setState[S](s: S): State[S, Unit] = State(_ =>((),s))
  
  def pureState[S, A](a: A): State[S, A] = State(s =>(a,s))

  def zipIndex[A](as: List[() => A]): List[(Int,A)] =
    as.foldLeft(
      pureState[Int, List[(Int,A)]](List()) // this machine outputs List() on every transition(which are all identities)
   )((acc, a) => {      // acc is state machine(starting with pure state above; a is an element of the list
       for {
         xs <- acc      // output of state machine, of type List[(Int,A)]]
         n  <- getState // output is int, because that's how we want it in line 34
         _  <- setState(n + 1)
       } yield (n,a())::xs
     }
   ).step(0)._1.reverse

  def even[A](ns: List[() => A]): Boolean =
    ns match {
      case Nil => true
      case x :: xs => odd({x(); xs})
    }
    
  def odd[A](ns : List[() => A]): Boolean =
    ns match {
      case Nil => false
      case x :: xs => even({x(); xs})
    }

  def run(): Unit = {
    try {
      val indexed = zipIndex(crashingList(250))
      mustFail("We expected an exception thrown")
    } catch {
      case ex: IllegalArgumentException =>
        val stacktrace = ex.getStackTrace
        val mainEntry = stacktrace(7)
        for {
          i <- 8 to 15
        } require(stacktrace(i) == mainEntry, s"Ex.1. We expected nine identical stacktrace rows, failed at $i")
    }
    println("Stacktrace demonstrates that we will have a stack overflow on large collections in zipIndex.")

    try {
      even(crashingList(250))
      mustFail("We expected an exception thrown")
    } catch {
      case ex: IllegalArgumentException =>
        val stacktrace = ex.getStackTrace
        require(stacktrace.size > 100, s"something wrong at ${stacktrace.size}: ${ex.getMessage}")
        val mainEntry = stacktrace(2)
        for { i <- 4 to 12 by 2 } {
          require(stacktrace(i) == mainEntry, s"Ex.2. We expected nine identical rows, failed at $i\n${stacktrace mkString "\n"}")
        }
    }
    println("Stacktrace demonstrates that we will have a stack overflow on large collections in mutual recursion.")
  }
  
}
