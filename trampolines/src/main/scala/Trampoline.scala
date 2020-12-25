import StateMachineWithTrampoline.mustFail
import StateMachineWithoutTrampoline.limitStack

import scala.annotation.tailrec

case class Fun[-A,+B](id: String, f: A => B) {
  def apply(a: A): B = f(a)
  override def toString: String = id
}

sealed trait Trampoline[+A] {
  @tailrec
  final def run: A = // it should be final or private for tailrec to work
    this match {
      case More(id, k) => k().run
      case Done(id, v) => v
    }

  def map[B](f: Fun[A, B]): Trampoline[B]
  def flatMap[B](f: Fun[A, Trampoline[B]]): Trampoline[B]
}

case class More[+A](id: String, k:() => Trampoline[A]) extends Trampoline[A] {
  def map[B](f: Fun[A, B]): More[B] = More(s"$f($id)", () => k() map f)
  def flatMap[B](f: Fun[A, Trampoline[B]]): Trampoline[B] = More(s"$f($id)", () => k() flatMap f)
}
// this is also pure()
case class Done[+A](id: String, result: A) extends Trampoline[A] {
  def map[B](f: Fun[A, B]): Done[B] = Done(s"$f($result)", f(result))
  def flatMap[B](f: Fun[A, Trampoline[B]]): Trampoline[B] = f(result)
}

object Trampoline extends Testing {
  def even[A](ns : List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done("even", true)
      case x :: xs =>
        limitStack(100, "Even")
        More("even", () => odd({ x(); xs }))
    }
  def odd[A](ns : List[() => A]): Trampoline[Boolean] =
    ns match {
      case Nil => Done("odd", false)
      case x :: xs => More("odd", () => even({ x(); xs }))
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
    println("Stacktrace demonstrates that we will NOT have a stack overflow on large collections in mutual recursion via Trampoline.")
  }
}