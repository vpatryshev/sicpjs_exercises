package onitut

import com.drew.imaging._
import com.drew.metadata.Metadata
import com.drew.metadata.file.FileSystemDirectory

import java.io.{File, IOException}
import java.nio.file.Files
import java.security.MessageDigest
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.language.postfixOps
import scala.util.Try

/**
 * Contains image files data handling
 */
object ImageFiles {

  private val ExifTimeFormat: DateTimeFormatter =
    DateTimeFormatter.ofPattern("EEE MMM d HH:mm:ss z yyyy")
  private val Extensions = ".*\\.jpg|jpeg"

  private val HashMethod = "MD5" // or "SHA" or "SHA-256"
  private val hasher: MessageDigest = MessageDigest.getInstance(HashMethod)
  
  def hashOf(bytes: Array[Byte]): String = hasher digest bytes map ("%02X" format _) mkString
  
  def hashOf(file: File): String = hashOf(Files readAllBytes file.toPath)

  /**
   * @param file link file
   * @return a list (maybe empty) of SymbolicLink records, or Nil if it's not a link
   */
  private def link(file: File): List[SymbolicLink] = {
    val path = file.toPath
    if (Files.isSymbolicLink(path)) {
      val to = Try {
        val target = Files.readSymbolicLink(path).toFile
        val size = target.length
        FileRecord(target)
      } toOption

      List(SymbolicLink(file, to))
    } else Nil
  }

  /**
   * Creates a FileRecord for a regular image file;
   * if the file is not a regular image, is empty, or cannot be read, we return Nil
   * 
   * @param file image file
   * @return A singleton list for an image file, or Nil
   */
  private def realFile(file: File): List[FileRecord] = {
    if (Files.isSymbolicLink(file.toPath) && file.canRead && file.length > 0) {
      try {
        List(FileRecord(file))
      } catch {
        case x: Exception =>
//          val fr = FileRecord(file)
//          throw x
          Nil
      }
    } else Nil
  }

  /**
   * Contents of a folder, deep, as Records.
   * @param folder the folder we scan
   * @return list of File/Link Records for this folder and subfolders
   */
  private def contents(folder: File): List[FileOrLink] = folder.listFiles.toList flatMap scan

  /**
   * Record(s) of a given file, which can be a link, a regular file, or a folder,
   * in which case we scan the whole tree.
   * 
   * @param file what we scan
   * @return a list of records for all image files or links to image files
   */
  def scan(file: File): List[FileOrLink] = {
    Try {
      if (file.isDirectory) contents(file)
      else if (file.getName.toLowerCase.matches(Extensions)) {
        if (Files.isSymbolicLink(file.toPath)) link(file)
        else realFile(file)
      } else Nil
    } getOrElse Nil
  }

  /**
   * Abstract record describing a file (image or link), or a group theirof, linked to one image file
   */
  trait Record extends Ordered[Record] {
    def id: String
    def files: List[(File, Long)]
    def size: Long
    def hash: String

    def paths: String = files.map(f => s"${f._1.getAbsoluteFile.getCanonicalPath}[${f._2}]") mkString ";"

    override def compare(that: Record): Int = paths compare that.paths
  }

  /**
   * Abstract record describing an image file or a link (for which target may be missing)
   */
  trait FileOrLink extends Record {
    def file: File

    def timestamp: Long = try {
      Files.getLastModifiedTime(file.toPath).toMillis
    } catch {
      case x: Exception =>
        throw x
    }
  }

  /**
   * Record describing a link to an image file
   * @param file link file
   * @param to image to which it points (may be missing)
   */
  case class SymbolicLink(file: File, to: Option[FileRecord]) extends FileOrLink {
    
    require(to.forall(_.timestamp > 0), s"wrong link for $file: $to")

    /**
     * Reverts the link: target becomes a link, this file becomes a target.
     * The contents of the target image is moved (as a file) to where the link was,
     * and is given the former link's name.
     */
    def revert(): Unit = for {
      rec <- to
      } {
        println("will revert $file to $to")
        val bak = new File(file.getAbsolutePath + ".bak01")
        file.renameTo(bak)
        rec.file.renameTo(file)
        Files.createSymbolicLink(rec.file.toPath, file.toPath)
        bak.delete()
     }

    /**
     * Identifies this record, taking the target id - or the file path if no target
     * @return record id
     */
    def id: String = hash
    
    def isValid: Boolean = to.isDefined

    /**
     * List of files with their timestamps
     * @return a list of pairs, (file, timestamp)
     */
    override def files: List[(File, Long)] = {
      (file, timestamp) :: (to map (_.files) getOrElse Nil)
    }

    /**
     * Checks whether this file is located somewhere inside a folder with the given path
     * @param folderPath for which we check it
     * @return true iff this file is inside the folder
     */
    def isInside(folderPath: String): Boolean =
      to exists (_.file.toPath startsWith folderPath)

    /**
     * @return target file size, or 0
     */
    def size: Long = to map (_.size) getOrElse 0
    
    def hash: String = to map (_.hash) getOrElse ("@" + file.toPath)
  }
  
  case class FileGroup(files: List[(File, Long)], size: Long, hash: String) extends Record {

    require(files.nonEmpty, "Empty file list not allowed")
    
    lazy val id: String = hash
    
    override def toString: String = s"$paths\t$size\t$hash"

    /**
     * Makes the first file in the list to be the image data file, and others must be links
     */
    def makeFirstFileLead(): Unit = {
      val main = files.head._1
      for (file <- files.tail.map(_._1)) {
        val bak = new File(file.getAbsolutePath + ".bak")
        file.renameTo(bak)
        try {
          Files.createSymbolicLink(file.toPath, main.toPath)
        } catch {
          case x: Exception =>
            System.err.println(s"failed to create link ${file.toPath} to ${main.toPath}: ${x.getMessage}")
        }
        bak.delete()
      }
    }
  }

  /**
   * Represents an image data file
   * @param file image data file
   */
  case class FileRecord(file: File) extends FileOrLink {
    require(!Files.isSymbolicLink(file.toPath), s"It's a link; need an actual file: $file")
    require(file.canRead, s"Not a readable file: $file")
    require(file.length > 0, s"empty file: $file")
    
    lazy val id: String = hash

    override def toString: String = s"$paths($exifTimestamp)\t$size\t$hash"

    override def compare(that: Record): Int = paths compare that.paths

    /**
     * @return Metadata of the image
     */
    def meta: Metadata = try {
      ImageMetadataReader.readMetadata(file)
    } catch {
      case x: Exception =>
        throw new IOException(s"Failed on file $file", x)
    }

    /**
     * @return image modification time from exif, as a string
     */
    def realModTime: String =
      meta.getFirstDirectoryOfType(classOf[FileSystemDirectory]).getString(3)

    /**
     * @return image timestamp, as ZonedDateTime
     */
    def exifTime: ZonedDateTime = ZonedDateTime.parse(realModTime, ExifTimeFormat)

    /**
     * @return image timestamp, in milliseconds
     */
    val exifTimestamp: Long = exifTime.toInstant.toEpochMilli

    /**
     * @return singleton list with this file and its file timestamp
     */
    override def files: List[(File, Long)] = List((file, timestamp))

    override def size: Long = file.length

    override def hash: String = ImageFiles hashOf file
  }
}