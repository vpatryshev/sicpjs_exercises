package onitut

import java.util.{Calendar, Date}
import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.{BasicFileAttributes, FileTime}
import scala.util.Try

object Lib:

  def log(msg: Any): Unit = println(msg)

  /**
   * Abstract record describing a file (image or link), or a group theirof, linked to one image file
   */
  trait HasId:
    /**
     * Identifies this record, taking the target id - or the file path if no target
     * @return record id
     */
    def id: String

  type Result[T] = Either[String, T]

  /**
   * Technical thing. Calculate a minimum of two optional values (or None, if none available)
   *
   * @param first  first optional value
   * @param second second optional value
   * @tparam T data type
   * @return minimum of two values, if present, or the one that is defined, or none
   */
  def min[T <: Comparable[T]](first: Option[T], second: Option[T]): Option[T] =
    (first :: second :: Nil).flatten match
      case Nil => None
      case list => Option(list.min)

  /**
   * Year of a timestamp
   */
  def year(timestamp: Long): Int =
    val calendar = Calendar.getInstance
    calendar.setTime(new Date(timestamp))
    calendar.get(Calendar.YEAR)

  def timestampOf(
    year: Int,
    month: Int = 0,
    day: Int = 1,
    hour: Int = 0,
    minute: Int = 0,
    second: Int = 0
  ): Long =
    val c: Calendar = Calendar.getInstance()
    c.set(year, month, day, hour, minute, second)
    c.getTime.getTime

  /**
   * Set the file's timestamp to what we found in exif
   */
  def setCreationTime(path: Path, millis: Long): Unit =
    val time = FileTime.fromMillis(millis)
    Files.setAttribute(path, "creationTime", time)
//    Files.setAttribute(path, "lastModifiedTime", time)

  def creationTime(path: Path): Option[Long] =
    val attr = Files.readAttributes(path, classOf[BasicFileAttributes])
    Option(attr).flatMap(attr => Option(attr.creationTime())).map(_.toMillis)

  def creationTimeSeconds(path: Path): Option[Long] = creationTime(path) map (_ / 1000)

  def fail(msg: String): Nothing =
    System.err.println(msg)
    System.exit(1)
    throw new NotImplementedError(msg)
