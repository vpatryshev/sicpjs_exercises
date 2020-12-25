import StateMachineWithoutTrampoline.limitStack

object StateMachineWithTrampoline extends Testing {
  
  case class State[S, +A](step : S => Trampoline[(A, S)]) {
    def map[B](f: A => B): State[S, B] = State(s => {
      val (done1, done2) = step(s).run
      Done("state", (f(done1), done2))
    })
    
    def altMap[B](f: A => B): State[S, B] =
      flatMap[B](a => pureState[S, B](f(a))) // must be same as map
    
    def flatMap[B](f: A => State[S,B]): State[S, B] =
      State[S,B](s => More("flat", () => {
        val (a, s1) = step(s).run
        More("flat1", () => f(a) step s1)
      }))
  }
    
  def pureState[S, A](a: A): State[S, A] = State[S, A] (s => Done("pure", (a, s)))

  def getState[S]: State[S, S] = State(s =>Done("get", (s,s)))
  
  def setState[S](s: S): State[S, Unit] = State(_ =>Done("set", ((),s)))

  def zipIndex[A](as: List[() => A]): List[(Int,A)] =
    as.foldLeft(
      pureState[Int, List[(Int,A)]](List()) // the machine outputs List() on every transition(which are all identities)
   )((acc, a) => { // acc is state machine(starting with pure state above; a is an element of the list
      limitStack(as.size - 2, s"before loop: $a")
      for {
        xs <- acc // output of state machine, of type List[(Int,A)]]
        ai = a()
//        _ = limitStack(as.size - 2, s"inside loop: $ai")
        i  <- getState // output is int, because that's how we want it in line 30
        _  <- setState(i + 1)
      } yield (i,ai)::xs
    }
   ).step(0).run._1.reverse

  def even[A](ns: List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil => 
        limitStack(90, "EVEN DONE")
        Done("even", true)
      case x :: xs => More(s"even $x", () => odd(xs))
    }
    
  def odd[A](ns : List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil =>
        limitStack(90, "ODD DONE")
        Done("odd", false)
      case x :: xs => More(s"odd $x", () => even(xs))
    }

  def run(): Unit = {

    try {
      val whoCares = even((1 until 200).map(i => () => i) toList)
      mustFail("We expected an exception thrown")
    } catch {
      case exex: IllegalArgumentException =>
        val stacktrace = exex.getStackTrace
        require(stacktrace.size < 20, s"Ex.3. We expected a short stack trace with trampoline\n${stacktrace mkString "\n"}")
    }
    println("Stacktrace demonstrates that we will not have a stack overflow on large collections in mutual recursion with trampoline.")

    try {
      val indexed = zipIndex(crashingList(200))
      mustFail("We expected an exception thrown")
    } catch {
      case exex: IllegalStateException =>
        val stacktrace = exex.getStackTrace
        println(s"This solution is not enough: we still have a deep stack (${stacktrace.length} entries)")
    }
    println("Stacktrace demonstrates that we will still have a stack overflow on large collections in zipIndex.")

  }
}


