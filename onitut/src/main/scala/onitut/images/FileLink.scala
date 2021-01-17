package onitut.images

import onitut.images.ImageFiles.FileRecord

import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.Date

sealed trait FileLink extends FileOrLink

case class BadSymbolicLink(override val path: Path, why: String) extends FileLink {
  def id: String = "@" + path

  override def toString = s"Bad Link $path: $why"

  override def size: Long = 0

  def fix(target: FileRecord): FileLink = {
    if (!Files.isSymbolicLink(path)) {
      println(s"failed to rename $path")
      this
    } else {
      val bak = makeItBak()
      Files.createSymbolicLink(path, target.path)
      Files.delete(bak)
      SymbolicLink(path, target)
    }
  }

}

/**
 * Record describing a link to an image file
 * @param path link path
 * @param to image file to which it points (may be missing)
 */
case class SymbolicLink(path: Path, to: FileRecord) extends FileLink {

  require(to.timestamp > Exif.MinPhotoTime, s"wrong link for $path: ${to.path}, ${new Date(to.timestamp)}")

  /**
   * Reverts the link: target becomes a link, this file becomes a target.
   * The contents of the target image is moved (as a file) to where the link was,
   * and is given the former link's name.
   */
  def revert(): Unit = {
    println("will revert $file to $to")
    val bak = makeItBak()
    Files.move(to.path, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
    Files.createSymbolicLink(to.path, path)
    Files.delete(bak)
  }

  /**
   * Checks whether this file is located somewhere inside a folder with the given path
   * @param folderPath for which we check it
   * @return true iff this file is inside the folder
   */
  def isInside(folderPath: String): Boolean =
    to.path startsWith folderPath

  /**
   * @return target file size, or 0
   */
  def size: Long = to.size

  def id: String = to.id
}
