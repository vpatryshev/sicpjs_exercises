package onitut.images

import onitut.Lib._
import java.nio.file.Files
import java.util.Date
import System.{err, out}

case class FileGroup(
  files: List[FileOrLink],
  id: String
) extends HasId with Ordered[FileGroup]:

  require(files.nonEmpty, "Empty file list not allowed")

  /**
   * paths of this group, with timestamps
   *
   * @return a comma-separated list of files with timestamps
   */
  lazy val paths: String =
    files.map(f => s"${f.path}[${f.date}]") mkString ";"

  override lazy val toString: String = s"Group($paths)"

  /**
   * Makes the first file in the list to be the image data file, and others will be links
   */
  def makeFirstFileLead(): Unit =
    val lead: FileOrLink = files.head
    for file <- files.tail do
      file.doWithBackup:  // make sure it's not lost in action
        try Files.createSymbolicLink(file.path, lead.path)
        catch case x: Exception =>
          err.println(s"failed to create link $file to $lead: ${x.getMessage}")

  def compare(that: FileGroup): Int = paths compare that.paths

