import scala.annotation.tailrec

sealed trait Trampoline[+A] {
  @tailrec
  final def runT: A =
    this match {
      case More(k) => k().runT
      case Done(v) => v
    }
}
case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

case class Done[+A](result: A) extends Trampoline[A]

object Trampoline {
  def main(args: Array[String]): Unit = {
    val crashingList: List[() => Int] = (0 to 20) map (n => () => {
      if (n > 10) throw new IllegalArgumentException("oops"); n*n
    }) toList
  }
}