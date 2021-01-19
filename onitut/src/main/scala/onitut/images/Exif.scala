package onitut.images

import com.drew.imaging.ImageMetadataReader
import com.drew.metadata.exif.{ExifIFD0Directory, ExifSubIFDDirectory}
import com.drew.metadata.{Directory, Metadata}

import java.io.IOException
import java.nio.file.Path
import java.util.{Calendar, Date, TimeZone}
import scala.reflect.ClassTag

/**
 * An adapter for Exif data of an image file
 * @param path an image file path
 */
case class Exif(path: Path) {
  /**
   * Metadata of the image
   */
  lazy val meta: Metadata =
    try ImageMetadataReader.readMetadata(path.toFile)
    catch { case x: Exception =>
      throw new IOException(s"Failed on file $path", x)
    }

  private def theClassOf[T:ClassTag]: Class[T] = implicitly[ClassTag[T]].runtimeClass.asInstanceOf[Class[T]]

  private def dir[T <: Directory : ClassTag]: Option[T] =
    Option(meta.getFirstDirectoryOfType(theClassOf[T]))

  val ourTimeZone: TimeZone = Calendar.getInstance.getTimeZone

  /**
   * @return image timestamp, as Date
   */
  def date: Option[Date] = {
    val date0 = filterTimestamp(dir[ExifIFD0Directory].flatMap(dir => Option(dir.getDate(306, ourTimeZone))))
    val subIFD = dir[ExifSubIFDDirectory]
    val date1 = filterTimestamp(subIFD flatMap (dir => Option(dir.getDateOriginal(ourTimeZone))))
    min(date0, date1)
  }

  /**
   * @return image timestamp, in milliseconds
   */
  val timestamp: Option[Long] = date map (_.getTime) filter (0 < _)

  def min[T <: Comparable[T]](first: Option[T], second: Option[T]): Option[T] = {
    (first, second) match {
      case (Some(a: T), Some(b: T)) => Some(if (a.compareTo(b) <= 0) a else b)
      case _ => first orElse second
    }
  }

  def filterTimestamp(ts: Option[Date]): Option[Date] =
    ts filter(_.getTime > Exif.TimeAfterWhichExifDateMakesSense)

  def dt(date0: Option[Date], date1: Option[Date]): Long = (date0, date1) match {
    case (Some(d0), Some(d1)) => Math.abs(d0.getTime - d1.getTime)
    case _ => 0L
  }
}

object Exif {
  def timeOfYear(year: Int): Long = {
    val c: Calendar = Calendar.getInstance()
    c.set(year, 0, 1)
    c.getTime.getTime
  }
  
  val MinPhotoTime: Long = timeOfYear(1820)
  
  val TimeAfterWhichExifDateMakesSense: Long = timeOfYear(2000)
}