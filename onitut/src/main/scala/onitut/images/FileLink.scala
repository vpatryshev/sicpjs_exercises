package onitut.images

import onitut.Lib.Result
import java.nio.file.{Files, Path, StandardCopyOption}
import java.nio.file.StandardCopyOption._
import java.util.Date

/**
 * Represents symbolic links to files with data
 */
sealed trait FileLink extends FileOrLink:
  def scold(msg: String): BadFileLink = BadFileLink(path, msg)

/**
 * Represents a bad symbolic link
 * @param path link path
 * @param why explanation of why it is bad
 */
case class BadFileLink(override val path: Path, why: String) extends FileLink:

  /**
   * We need an id, but we can't produce the file's hash.
   *  @return record id
   */
  lazy val id: String = "@" + path

  override lazy val toString = s"Bad Link $path: $why"

  /**
   * Try to fix this bad link
   * @param target the new target for this symbolic link
   * @return a new FileLink instance (it's good now)
   */
  def fix(target: FileRecord): FileLink =
    if (!Files.isSymbolicLink(path))
      BadFileLink(path, s"it's not a symbolic link")
    else doWithBackup:
      Files.createSymbolicLink(path, target.path)
      SymbolicLink(path, target)

/**
 * Record describing a link to an image file
 * @param path link path
 * @param to image file to which it points (may be missing)
 */
case class SymbolicLink(path: Path, to: FileRecord, depth: Int = 1) extends FileLink:
  def redirectTo(target: FileRecord): SymbolicLink =
    doWithBackup:
      SymbolicLink(Files.createSymbolicLink(path, target.path), target)

  require(
    to.timestamp > Exif.MinPhotoTime,
    s"wrong link for $path: ${to.path}, ${to.date}"
  )
  
  /**
   * Removes a chain of links, pointing directly to the target file
   */
  def resolve(): Unit =
    if  depth > 1 then
      doWithBackup:
        Files.createSymbolicLink(path, to.path)

  /**
   * Reverts the link: target becomes a link, this file becomes a target.
   * The contents of the target image is moved (as a file) to where the link was,
   * and is given the former link's name.
   */
  def revert(): Unit =
    doWithBackup:
      Files.move(to.path, path, REPLACE_EXISTING, COPY_ATTRIBUTES)
      Files.createSymbolicLink(to.path, path)
      println(s"reverted $path to $to")

  /**
   * Checks whether this file is located somewhere inside a folder with the given path
   * @param folderPath for which we check it
   * @return true iff this file is inside the folder
   */
  def isInside(folderPath: Path): Boolean =
    to.path.startsWith(folderPath)

  def id: String = to.id

