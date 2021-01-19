package onitut.images

import onitut.images.ImageFiles.makeBak

import java.nio.file.{Files, Path}
import java.util.Date

case class FileGroup(files: List[(Path, Long)], size: Long, id: String) extends Record with Ordered[FileGroup]{

  require(files.nonEmpty, "Empty file list not allowed")

  def paths: String = files.map(f => s"${f._1}[${new Date(f._2)}]") mkString ";"

  override def toString: String = s"$paths\t$size\t$id"

  /**
   * Makes the first file in the list to be the image data file, and others must be links
   */
  def makeFirstFileLead(): Unit = {
    val main = files.head._1
    for (path <- files.tail.map(_._1)) {
      val bak = makeBak(path)
      try {
        Files.createSymbolicLink(path, main)
      } catch {
        case x: Exception =>
          System.err.println(s"failed to create link $path to $main: ${x.getMessage}")
      }
      Files.delete(bak)
    }
  }

  def compare(that: FileGroup): Int = paths compare that.paths
}
