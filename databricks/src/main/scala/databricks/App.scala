package databricks

import java.io.File

import scalaz.Alpha.{F, T}
import scalaz.Forall.A
import scalaz.{Monad, OptionT}

object App {
  lazy val countriesFile = new File("assignment-data/country.csv")
  lazy val countries = DataSet.read(countriesFile)

  lazy val citiesFile = new File("assignment-data/city.csv")
  lazy val cities = DataSet.read(citiesFile)

  lazy val languagesFile = new File("assignment-data/language.csv")
  lazy val languages = DataSet.read(languagesFile)
  
  val allData: Map[String, DataSet] = Set(countries, cities, languages) map (set => set.name -> set) toMap
  
  private def join(set: DataSet, others: List[(String, String)]): DataSet = others match {
    case Nil => set
    case (dsName, colName)::tail => 
      val firstJoin: DataSet = DataSet.join(set, colName, allData(dsName), colName)
      join(firstJoin, tail)
  }
  
  def interpret(queryString: String): DataSet = {
    val query = queryString.split(" ").toList
    require(query.head == "FROM")
    val next = Set("SELECT", "COUNTBY", "ORDERBY", "TAKE")
    val sets = query.tail.takeWhile(word => !next.contains(word))
    val joins = sets mkString " " split " JOIN " toList
    
    val first = allData(joins.head.trim)
    val otherNames: List[(String, String)] = joins.tail.map(pair => {
      val dsname::colname::_ = pair.split(" ").toList
      (dsname, colname)
    })
    val source = join(first, otherNames)
      
    val specs = query.dropWhile(word => !next.contains(word))
    println(s"$queryString\n${source take 5}\n$specs")
    
    exec(source, specs)
  }
  
  private def exec(ds: DataSet, specs: List[String]): DataSet = {
    specs match {
      case "SELECT"::keys::tail => exec(ds.select(keys), tail)
      case "COUNTBY"::key::tail => exec(ds.countBy(key), tail)
      case "ORDERBY"::key::tail => exec(ds.orderBy(key), tail)
      case "TAKE"::howmany::tail => exec(ds.take(howmany.toInt), tail)
      case Nil => ds
      case other =>
        throw new IllegalArgumentException(s"oops, error: $other");
    }
  }
  
  def main(args: Array[String]): Unit = {
    case class P[T](x: T, y:T) {
      def map[U](f: T => U): P[U] = P(f(x), f(y))
      def flatMap[U](f: T => P[U]): P[U] = P(f(x).x, f(y).y)
    }
    def flatten[T](ppt: P[P[T]]): P[T] = ppt.flatMap[T](identity)
    def unit[T](x: T): P[T] = P[T](x, x)

    case class OptionP[T](pt: P[Option[T]]) {
      def map[U](f: T => U): OptionP[U] = OptionP[U](pt.map(xOpt => xOpt.map(f)))
//      def flatMap[U](f: T => OptionP[U]): OptionP[U] = pt.map(xOpt => xOpt.map(f))
    def flatMap[U](f: T => OptionP[U]): OptionP[T] = {
      val ptf: T => P[Option[U]] = (t:T) => f(t).pt 
      val fm: (Option[T] => P[U]) => P[U] = pt.flatMap[U]
      
//      def ff(pto: P[Option[T]]): P[Option[U]] = 
//      OptionP(pt.flatMap(_.fold(unit[Option[T]](None))(f))
    }

      def unit[T](x: T): OptionP[T] = OptionP[T](P[Option[T]](Option(x), Option(x)))
    }
    
    /*
      def flatMap[B](f: A => OptionT[F, B])(implicit F: Monad[F]): OptionT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[Option[B]])(implicit F: Monad[F]): OptionT[F, B] =
    OptionT(F.flatMap(value)(_.fold(F.pure[Option[B]](None))(f)))

     */
    
    //    while(true) println(interpret(scala.io.StdIn.readLine(">")))
  }
  
}
