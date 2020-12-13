trait Testing {
  def mustFail(message: String): Unit = require(requirement = false, message)

  def crashingList(size: Int): List[() => Int] =(0 to size) map(i =>() => {
    if(i > size - 2) throw new IllegalArgumentException(s"oops at $i/$size"); i*i
  }) toList

  def run(): Unit

  def main(args: Array[String]): Unit = run()
}