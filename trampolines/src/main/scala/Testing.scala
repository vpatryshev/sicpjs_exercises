import java.lang.Comparable

trait Testing extends StackControl {
  def mustFail(message: String): Unit = require(requirement = false, message)

  
  def crashingList(size: Int): List[() => Int] =(0 to size) map(i =>() => {
//    limitStack(size - 2: Int, s"inside list[$i]")
    if (i > 100) throw new IllegalStateException(s"Ok, list crashed, with stack size $depth")
    i*i
  }) toList
  
  def run(): Unit

  def main(args: Array[String]): Unit = run()
}

object TT extends Testing {
  def run(): Unit = {
    println(analyze(Array(1,2,3,2,3,2,3,4,5,2,3)))
    println(analyze(Array(1,2,3,2,2,2,2,4,5,2,3)))
  }
}
