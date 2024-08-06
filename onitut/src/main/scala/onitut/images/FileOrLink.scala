package onitut.images

import onitut.images.ImageFiles.makeBak
import onitut.Lib
import onitut.Lib._
import java.nio.file.{Files, Path}
import java.util.{Calendar, Date}

/**
 * Abstract record describing an image file or a link (for which target may be missing)
 */
trait FileOrLink extends HasId:
  
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
  private def makeItBak(): Path = makeBak(path)

  /**
   * Checks if the file is inside `thumbnails` folder; its for the web, and nobody cares about thumbnails
   * @return true if so
   */
  def isThumbnail: Boolean = path.toString.contains("/thumbnails/")

  /**
   * Timestamp of a file (last modified time, in millis)
   */
  lazy val timestamp: Long = Files.getLastModifiedTime(path).toMillis

  lazy val date: Date = new Date(timestamp)

  lazy val year: Int = Lib.year(timestamp)

  /**
   * Do something on a link while backing it up just in case; in the end delete the backup.
   * @param op operation we want to run, returns of value of type `T`
   * @tparam T type of result
   * @return whatever result the operation produced - or an exception happens.
   */
  def doWithBackup[T](op: => T): T =
    val bak = makeItBak()
    val result = op
    if (System.getProperty("onitut.keepBakFiles", "false").toBoolean)
      Files.delete(bak)

    result

