import java.lang.Comparable
import java.util.Objects

trait Testing {
  def mustFail(message: String): Unit = require(requirement = false, message)

  def stack: Array[StackTraceElement] = Thread.currentThread().getStackTrace
  def depth: Int = stack.length
  
  case class Analysis(start: Int, period: Int, repetitions: Int) extends Comparable[Analysis] {
    override def compareTo(that: Analysis): Int = {
      repetitions compareTo that.repetitions match {
        case c if c != 0 => c
        case 0 =>
          period compareTo that.period match {
            case c if c != 0 => c
            case 0 => that.start compareTo start
          }  
      }
    }
  }
  
  def analyze[T](data: Array[T]): Analysis = {
    val n = data.length
    
    def repetitions(start: Int, step: Int): Int = {
      def same(i1: Int) = 0 until step forall (i => data(start+i) == data(i1 + i))

      for {
        j <- start + step until n-step by step
      } {
        if (!same(j)) return (j - start) / step
      }
      1
    }
    
    (for {
      start <- 0 until n / 2
      step <- 1 until (n - start) / 2
      reps <- Option(repetitions(start, step)) filter (1 <)
//      _ = println(s"start $start, step $step, reps $reps")
    } yield Analysis(start, step, reps)).max
  }
  
  def limitStack(n: Int, context: Any): Unit = {
    require(depth < n, {
//      println(stack mkString ("\n"))
      val analysis = analyze(stack)
      s"$context. Crashing as stack depth = $depth, $analysis"
    })
  }
  
  def crashingList(size: Int): List[() => Int] =(0 to size) map(i =>() => {
    limitStack(size - 2: Int, i)
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
