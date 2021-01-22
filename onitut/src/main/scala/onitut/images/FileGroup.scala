package onitut.images

import java.nio.file.Files
import java.util.Date

case class FileGroup(files: List[FileOrLink], id: String) extends Record with Ordered[FileGroup]{

  require(files.nonEmpty, "Empty file list not allowed")

  /**
   * paths of this group, with timestamps
   * @return a comma-separated list of files with timestamps
   */
  def paths: String = files.map(f => s"${f.path}[${new Date(f.timestamp)}]") mkString ";"

  override def toString: String = s"$paths\t$id"

  /**
   * Makes the first file in the list to be the image data file, and others must be links
   */
  def makeFirstFileLead(): Unit = {
    val main: FileOrLink = files.head
    for (file <- files.tail) {
      file.doWithBackup {
        try Files.createSymbolicLink(file.path, main.path)
        catch {
          case x: Exception =>
            System.err.println(s"failed to create link $file to $main: ${x.getMessage}")
        }
      }
    }
  }

  def compare(that: FileGroup): Int = paths compare that.paths
}
