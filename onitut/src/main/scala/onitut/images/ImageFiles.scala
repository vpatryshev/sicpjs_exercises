package onitut.images

import java.io.File
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.security.MessageDigest
import java.util.{Calendar, Date}
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Try

/**
 * Contains image files data handling
 */
object ImageFiles {
  private val Extensions = ".*\\.jpg|jpeg"

  private val HashMethod = "MD5" // or "SHA" or "SHA-256"
  private val hasher: MessageDigest = MessageDigest.getInstance(HashMethod)
  
  def hashOf(bytes: Array[Byte]): String = hasher digest bytes map ("%02X" format _) mkString
  
  def hashOf(path: Path): String = hashOf(Files readAllBytes path)
  
  @tailrec
  def resolveLink(paths: List[Path]): Either[String, Path] = {
    paths match {
      case path::_ if Files.isSymbolicLink(path) =>
        val target: Path = Files.readSymbolicLink(path)
        if (paths.contains(target)) Left("loop")
        else resolveLink(target::paths)
      case _ => {
        paths.headOption filter Files.isReadable toRight "missing"
      }
    }
  }

  /**
   * @param file link file
   * @return a list (maybe empty) of SymbolicLink records, or Nil if it's not a link
   */
  private def link(file: File): FileLink = {
    val path = file.toPath
    if (Files.isSymbolicLink(path)) {
      val to: Either[String, FileRecord] = Try {
        val target = resolveLink(path::Nil)
        target map FileRecord
      } .toEither.left.map(_.getMessage).flatten
      
      to match {
        case Left(err) => BadSymbolicLink(path, err)
        case Right(target) => SymbolicLink(path, target)
     }
    } else BadSymbolicLink(path, "not a link")
  }

  /**
   * Creates a FileRecord for a regular image file;
   * if the file is not a regular image, is empty, or cannot be read, we return Nil
   * 
   * @param file image file
   * @return A singleton list for an image file, or Nil
   */
  private def realFile(file: File): Option[FileRecord] = {
    val path = file.toPath
    if (!Files.isSymbolicLink(path) && file.canRead && file.length > 0) {
      try {
        Option(FileRecord(path))
      } catch {
        case x: Exception =>
          val fr = FileRecord(path)
//          throw x
          None
      }
    } else None
  }

  // it's slightly cheaper than map/reduce
  def fold[T](op: File => Option[T]): File => Iterable[T] = {

    def scan(file: File): Iterable[T] = {
      Try {
        if (file.isDirectory) file.listFiles.toList flatMap scan
        else op(file).toList
      } getOrElse Nil
    }

    scan
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
  def scan(file: File): List[FileOrLink] = fold(
    file => if (!file.getName.toLowerCase.matches(Extensions)) None else
            if (Files.isSymbolicLink(file.toPath)) Option(link(file))
            else realFile(file)
          )(file) toList

  def makeBak(path: Path): Path = {
    val bak = Paths.get(path + ".bak")
    Files.move(path, bak, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
    bak
  }

  
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

  /**
   * Represents an image data file
   * @param path image data file
   */
  case class FileRecord(path: Path) extends FileOrLink {
    val file: File = path.toFile

    def touch(): Unit = exifTimestamp foreach {
      ts => try file.setLastModified(ts)
            catch {
              case x: Exception =>
                println(s"$x on $this")
            }
    }

    def folderYear(photoDir: String): Option[Int] =
      Try(file.getPath.slice(photoDir.length + 1, photoDir.length + 5).toInt) toOption
    
    def moveToItsYear(): Unit = {
       ???            
    }

    def hasProblemWithTimestamp: Boolean = {
      val yes = !isOld && exifTimestamp.exists (ts => Math.abs(ts - timestamp) > 1800000)
      yes
    }

    def fileTimestampDoneLater: Boolean = {
      val yes = !isOld && exifTimestamp.exists (timestamp + 3601000 >)
      yes
    }

    require(!Files.isSymbolicLink(file.toPath), s"It's a link; need an actual file: $file")
    require(file.canRead, s"Not a readable file: $file")
    require(file.length > 0, s"empty file: $file")
    
    def shortString: String = s"$file(${new Date(exifTimestamp.getOrElse(timestamp))})"

    override def toString: String = {
      s"$shortString\t$size\t$id"
    }

    /**
     * @return Exif of the image
     */
    lazy val exif: Exif = Exif(path)

    /**
     * @return image timestamp, in milliseconds
     */
    lazy val exifTimestamp: Option[Long] = exif.timestamp
    
    def isOld: Boolean = timestamp < Exif.TimeAfterWhichExifDateMakesSense

    override def size: Long = Files size path

    override lazy val id: String = ImageFiles hashOf path

  }
}
