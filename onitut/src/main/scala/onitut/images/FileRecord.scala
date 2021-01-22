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

  def touch(): Unit = exifTimestamp foreach {
    ts => try Files.setLastModifiedTime(path, FileTime.fromMillis(ts))
    catch {
      case x: Exception =>
        println(s"$x on $this")
    }
  }

  def folderYear(photoDir: Path): Option[Int] = {
    val res = Try(photoDir.relativize(path).toString.substring(0, 4).toInt).toOption
    res
  }

  def moveToItsYear(photoDir: Path, links: List[SymbolicLink]): (FileRecord, List[SymbolicLink]) = {
    val yearPath = photoDir.resolve(year.toString)
    val newPath = yearPath.resolve(name)
    if (Files.isRegularFile(newPath) && Files.isReadable(newPath)) {
      throw new IllegalArgumentException(s"File $newPath already exists, can't move $path there")
    }
    if (!path.startsWith(yearPath)) {
      if (Files.isSymbolicLink(newPath)) Files.delete(newPath)
      Files.move(path, newPath)
      val newRecord = FileRecord(newPath)
      (newRecord, links.map (_.redirectTo(newRecord)))
    } else (this, links)
  }

  def hasProblemWithTimestamp: Boolean = {
    val yes = !isOld && exifTimestamp.exists (ts => Math.abs(ts - timestamp) > 1800000)
    yes
  }

  def fileTimestampDoneLater: Boolean = {
    val yes = !isOld && exifTimestamp.exists (timestamp + 3601000 >)
    yes
  }

  require(!Files.isSymbolicLink(path), s"It's a link; need an actual file: $path")
  require(Files.isReadable(path), s"Not a readable file: $path")
  require(Files.size(path) > 0, s"empty file: $path")

  override def toString: String = s"$path(${new Date(exifTimestamp.getOrElse(timestamp))})\t$size"

  /**
   * @return Exif of the image
   */
  lazy val exif: Exif = Exif(path)

  /**
   * @return image timestamp, in milliseconds
   */
  lazy val exifTimestamp: Option[Long] = exif.timestamp

  def isOld: Boolean = timestamp < Exif.TimeAfterWhichExifDateMakesSense

  def size: Long = Files size path

  override lazy val id: String = ImageFiles hashOf path

}
