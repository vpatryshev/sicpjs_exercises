package onitut.images

import java.nio.file.attribute.FileTime
import java.nio.file.{Files, Path, Paths}
import java.util.Date
import scala.util.Try

/**
 * Represents an image data file
 *
 * @param path image data file
 */
case class FileRecord(path: Path) extends FileOrLink {

  require(!Files.isSymbolicLink(path), s"It's a link; need an actual file: $path")
  require(Files.isReadable(path), s"Not a readable file: $path")
  require(Files.size(path) > 0, s"empty file: $path")

  /**
   * Set the file's timestamp to what we found in exif
   */
  def touch(): Unit = exifTimestamp foreach {
    ts => try Files.setLastModifiedTime(path, FileTime fromMillis ts)
    catch { case x: Exception => println(s"$x on $this") }
  }

  /**
   * If the folder name is year number, that's what we return (in Option), or None
   * @param photoDir folder holding all photos
   * @return the year, as an int
   */
  def folderYear(photoDir: Path): Option[Int] = {
    Try(photoDir.relativize(path).toString.substring(0, 4).toInt).toOption
  }

  /**
   * Moves this image file in photoDir to the right year folder
   * @param photoDir folder holding all photos
   * @param links links to this image file, from everywhere
   * @return a pair, a new FileRecord (with moved file) and a list of updated symbolic links.
   */
  def moveToItsYear(photoDir: Path, links: List[SymbolicLink]): (FileRecord, List[SymbolicLink]) = {
    val yearPath = photoDir.resolve(year.toString)
    val newPath = yearPath.resolve(name)
    
    if (Files.isRegularFile(newPath) && Files.isReadable(newPath) && path != newPath) {
      System.err.println(s"File $newPath already exists, can't move $path there")
    }
    if (!path.startsWith(yearPath)) {
      if (Files.isSymbolicLink(newPath)) Files.delete(newPath)
      Files.move(path, newPath)
      val newRecord = FileRecord(newPath)
      (newRecord, links map (_.redirectTo(newRecord)))
    } else (this, links)
  }

  /**
   * Checks whether this file has problems with timestamp
   * @return true if this is the case
   */
  def hasProblemWithTimestamp: Boolean = {
    val yes = !isOld && exifTimestamp.exists (ts => Math.abs(ts - timestamp) > 1800000)
    yes
  }

  /**
   * Checks whether the file timestamp is too new compared to exif timestamp
   * @return
   */
  def fileTimestampDoneLater: Boolean = {
    val yes = !isOld && exifTimestamp.exists (timestamp + 3601000 >)
    yes
  }

  /**
   * @return Exif of the image
   */
  lazy val exif: Exif = Exif(path)

  /**
   * @return image timestamp, in milliseconds
   */
  lazy val exifTimestamp: Option[Long] = exif.timestamp

  /**
   * Checks whether the file is old: its exif can't contain the right timestamp then
   * @return true if this is the case
   */
  def isOld: Boolean = timestamp < Exif.TimeAfterWhichExifDateMakesSense

  /**
   * @return size of this file
   */
  def size: Long = Files size path

  override lazy val id: String = ImageFiles hashOf path

  override def toString: String = s"$path(${new Date(exifTimestamp.getOrElse(timestamp))})\t$size"

}
