trait Testing {
  def mustFail(message: String): Unit = require(requirement = false, message)

  def depth: Int = Thread.currentThread().getStackTrace.length
  
  def limitStack(n: Int, context: Any): Unit = {
    require(depth < n, s"$context. Crashing as stack depth = $depth")
  }
  
  def crashingList(size: Int): List[() => Int] =(0 to size) map(i =>() => {
    limitStack(size - 2: Int, i)
    i*i
  }) toList

  def run(): Unit

  def main(args: Array[String]): Unit = run()
}