package onitut.images

import onitut.images.ImageFiles.makeBak

import java.nio.file.{Files, Path}
import java.util.{Calendar, Date}

/**
 * Abstract record describing an image file or a link (for which target may be missing)
 */
trait FileOrLink extends Record {
  def path: Path

  def makeItBak(): Path = makeBak(path)

  // TODO: fix it
  def isThumbnail: Boolean = path.toString.contains("/thumbnails/")

  def timestamp: Long = try {
    Files.getLastModifiedTime(path).toMillis
  } catch {
    case x: Exception =>
      throw x
  }

  lazy val year: Int = {
    val calendar = Calendar.getInstance
    calendar.setTime(new Date(timestamp))
    calendar.get(Calendar.YEAR)
  }
}
