package onitut.images

import onitut.images.ImageFiles.makeBak

import java.nio.file.{Files, Path}
import java.util.{Calendar, Date}

/**
 * Abstract record describing an image file or a link (for which target may be missing)
 */
trait FileOrLink extends Record {
  
  /**
   * Path of this file (or link
   * @return the path
   */
  val path: Path

  /**
   * File name
   * @return file name
   */
  lazy val name: String = path.getFileName.toString

  /**
   * Rename this file to a .bak file - to make sure it's not lost in action
   * @return path for the renamed file
   */
  def makeItBak(): Path = makeBak(path)

  /**
   * Checks if the file is inside `thumbnails` folder; its for the web, and nobody cares about thumbnails
   * @return true if so
   */
  def isThumbnail: Boolean = path.toString.contains("/thumbnails/")

  /**
   * Timestamp of a file (last modified time, in millis)
   */
  lazy val timestamp: Long = Files.getLastModifiedTime(path).toMillis

  /**
   * Year of a file, from its `last modified time`
   */
  lazy val year: Int = {
    val calendar = Calendar.getInstance
    calendar.setTime(new Date(timestamp))
    calendar.get(Calendar.YEAR)
  }

  /**
   * Do something on a link while backing it up just in case; in the end delete the backup.
   * @param op operation we want to run, returns of value of type `T`
   * @tparam T type of result
   * @return whatever result the operation produced - or an exception happens.
   */
  def doWithBackup[T](op: => T): T = {
    val bak = makeItBak()
    val result = op
    Files.delete(bak)
    result
  }

}
