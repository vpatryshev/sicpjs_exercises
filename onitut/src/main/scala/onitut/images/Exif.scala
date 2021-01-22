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
   * Extract image timestamp. Since there may be more than one, we take the earliest.
   * 
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

  /**
   * Technical thing. Calculate a minimum of two optional values (or None, if none available)
   * @param first first optional value
   * @param second second optional value
   * @tparam T data type
   * @return minimum of two values, if present, or the one that is defined, or none
   */
  def min[T <: Comparable[T]](first: Option[T], second: Option[T]): Option[T] = {
    (first, second) match {
      case (Some(a: T), Some(b: T)) => Some(if (a.compareTo(b) <= 0) a else b)
      case _ => first orElse second
    }
  }

  /**
   * Making sure that the date we have is reasonable. Exif dates did not exist before year 2k.
   * @param ts the optional timestamp we found in exif, may be wrong (e.g. year 0000)
   * @return an optional timestamp; must be valid
   */
  private def filterTimestamp(ts: Option[Date]): Option[Date] =
    ts filter(_.getTime > Exif.TimeAfterWhichExifDateMakesSense)
}

object Exif {
  private def timeOfYear(year: Int): Long = {
    val c: Calendar = Calendar.getInstance()
    c.set(year, 0, 1)
    c.getTime.getTime
  }

  /**
   * Minimum time when the photo might have been taken.
   * @see https://en.wikipedia.org/wiki/History_of_photography
   */
  val MinPhotoTime: Long = timeOfYear(1826)

  /**
   * Minimum time when exif started accompanying photos
   * @see https://en.wikipedia.org/wiki/Exif#Background
   */
  val TimeAfterWhichExifDateMakesSense: Long = timeOfYear(1998)
}