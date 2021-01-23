package onitut.images

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.Try

/**
 * Image files data handling
 */
object ImageFiles {

  lazy val HomeDir: Path = Paths.get(System.getProperty("user.home"))

  private val Extensions = ".*\\.jpg|jpeg"

  private val HashMethod = "MD5" // or "SHA" or "SHA-256"
  private val hasher: MessageDigest = MessageDigest.getInstance(HashMethod)
  
  def hashOf(bytes: Array[Byte]): String = hasher digest bytes map ("%02X" format _) mkString
  
  def hashOf(path: Path): String = hashOf(Files readAllBytes path)
  
  def resolveLink(path: Path): Either[String, (Path, Int)] = {
    @tailrec def trace(paths: List[Path]): Either[String, (Path, Int)] = {
      paths match {
        case path :: _ if Files.isSymbolicLink(path) =>
          val target: Path = Files.readSymbolicLink(path)
          if (paths.contains(target)) Left("circular loop")
          else trace(target :: paths)

        case path :: _ if Files.isReadable(path) => Right((path, paths.length - 1))
        case _                                   => Left("missing")
      }
    }
      
    trace(path::Nil)
  }

  /**
   * @param path link file path
   * @return a list (maybe empty) of SymbolicLink records, or Nil if it's not a link
   */
   def link(path: Path): FileLink = {
    if (Files.isSymbolicLink(path)) {
      val to: Either[String, (FileRecord, Int)] = Try {
        val target = resolveLink(path)
        target map { case (p, depth) => (FileRecord(p), depth) }
      } .toEither.left.map(_.getMessage).flatten
      
      to match {
        case Left(err) => BadSymbolicLink(path, err)
        case Right((target, depth)) => SymbolicLink(path, target, depth)
     }
    } else BadSymbolicLink(path, "not a link")
  }

  /**
   * Creates a FileRecord for a regular image file;
   * if the file is not a regular image, is empty, or cannot be read, we return Nil
   * 
   * @param path image file path
   * @return A singleton list for an image file, or Nil
   */
  private def realFile(path: Path): Option[FileRecord] = {
    if (!Files.isSymbolicLink(path) &&
         Files.isReadable    (path) &&
         Files.size          (path) > 0)
         Try { FileRecord(path) } toOption
    else None
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
  def scan(file: File): List[FileOrLink] = traverse(
    f => {
      val path = f.toPath
      if (!path.getFileName.toString.toLowerCase.matches(Extensions)) None else
        if (Files.isSymbolicLink(path)) Option(link(path))
        else realFile(path)
    }
          )(file) toList

  /**
   * Given a path, add `.bak` to its extension.
   * First attempt to do it with COPY_ATTRIBUTES; if failed, just rename.
   * @param path path of the file
   * @return a new path, of the file that is renamed
   */
  def makeBak(path: Path): Path = {
    val bak = Paths.get(path + ".bak")
    try {
      Files.move(path, bak, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.COPY_ATTRIBUTES)
    } catch {
      case oops: UnsupportedOperationException =>
        Files.move(path, bak, StandardCopyOption.REPLACE_EXISTING)
    }
    bak
  }

  /**
   * Groups all entries by their hash
   * @param entries data we scan
   * @return a list of grouped data
   */
  def analyze(entries: List[FileOrLink]): List[FileGroup] = {

    val grouped: Map[String, List[FileOrLink]] = entries.groupBy(_.id)

    grouped.values.map(list => {
      val files: List[FileOrLink] = list sortBy (_.timestamp)
      
      FileGroup(files, list.head.id)
    }).toList sorted
  }

  /**
   * Dumps a list of records to a file
   * @param data list of records
   * @param path path of the output file
   */
  def dump(data: List[Record], path: String): Unit = {
    val out = new PrintWriter(new FileWriter(path))
    data foreach out.println
    out.close()
  }

  /**
   * group scanned photos by year
   * @param scannedPhotos list of files (or links?)
   * @param photoDir the folder where all phostos are supposed to be stored
   * @return a pair: a map: year -> list of photos, and a sequence of undated photos
   */
  def groupByYear(scannedPhotos: List[FileOrLink], photoDir: Path): (Map[Int, List[FileRecord]], Seq[FileRecord]) = {
    val photosWithYearsMaybe = scannedPhotos.collect {
      case fr: FileRecord => (fr.folderYear(photoDir), fr)
    }

    val datedPhotos: List[(Int, FileRecord)] = photosWithYearsMaybe collect {
      case (Some(year: Int), fr) => year -> fr
    }

    val undatedPhotos: Seq[FileRecord] = photosWithYearsMaybe collect {
      case (None, fr) => fr
    }
    val mapByYear = (datedPhotos groupBy (_._1)).view.mapValues(_.map(_._2)).toMap

    (mapByYear, undatedPhotos)
  }

  /**
   * Reverts links pointing outside of our photo folder
   * @param files all (image) files in our photo folder
   * @param photoDir photo pholder path
   */
  def revertExternalLinks(files: List[FileOrLink], photoDir: Path): Unit = {
    // links in our pictures folder pointing outside
    val externalPhotoLinks = files collect {
      case link: SymbolicLink if !(link isInside photoDir) => link
    }

    // revert them all, so all images are in pictures folder
    externalPhotoLinks foreach (_.revert())
  }

  /**
   * Traverses the collection of files inside the folder, recursively.
   * @param op whatever wit do on the file
   * @tparam T type of returned data, per file
   * @return an iterable of `T`s.
   */
  def traverse[T](op: File => Option[T]): File => Iterable[T] = {

    def scan(file: File): Iterable[T] = {
      Try {
        if (file.isDirectory) file.listFiles.toList flatMap scan
        else op(file).toList
      } getOrElse Nil
    }

    scan
  }


}
