import StateMachineWithTrampoline.mustFail

import scala.annotation.tailrec

sealed trait Trampoline[+A] {
  @tailrec
  final def run: A = // it should be final or private for tailrec to work
    this match {
      case More(k) => k().run
      case Done(v) => v
    }

  def map[B](f: A => B): Trampoline[B]
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B]
}

case class More[+A](k:() => Trampoline[A]) extends Trampoline[A] {
  def map[B](f: A => B): More[B] = More(() => k() map f)
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = More(() => k() flatMap f)
}
// this is also pure()
case class Done[+A](result: A) extends Trampoline[A] {
  def map[B](f: A => B): Done[B] = Done(f(result))
  def flatMap[B](f: A => Trampoline[B]): Trampoline[B] = f(result)
}

object Trampoline extends Testing {
  def even[A](ns : List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(true)
      case x :: xs => More(() => odd({ x(); xs }))
    }
  def odd[A](ns : List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done(false)
      case x :: xs => More(() => even({ x(); xs }))
    }
  
  def run(): Unit = {
    try {
      val whoCares = even(crashingList(200)).run
      mustFail("We expected an exception thrown")
    } catch {
      case exex: IllegalArgumentException =>
        val stacktrace = exex.getStackTrace
        require(stacktrace.size < 13, s"Ex.3. We expected a short stack trace with trampoline\n${stacktrace mkString "\n"}")
    }
    println("Stacktrace demonstrates that we will NOT have a stack overflow on large collections in mutual recursion.")
  }
}