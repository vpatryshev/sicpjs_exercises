package onitut

import java.io.{File, FileWriter, PrintWriter}
import java.nio.file.Files
import java.text.SimpleDateFormat
import java.util.Date

import onitut.ImageFiles._

import scala.language.postfixOps
import scala.util.Try

/**
 * Manages image files across the drive
 */
object Main {
  val DateFormat: SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
  val Extensions = ".*\\.jpg|jpeg"
  
  def fail(msg: String): Nothing = {
    System.err.println(msg)
    System.exit(1)
    throw new NotImplementedError(msg)
  }

  /**
   * This is the mac standard, pictures are in ~/Pictures
   */
  val DefaultPhotoDir: String = {
    val home = System.getProperty("user.home")
    new File(new File(home), "Pictures").getAbsoluteFile.getCanonicalPath
  }
  
  val DefaultListFile: String = "filelist.out"

  /**
   * 
   * @param args root: folder that we scan; photoDir: your pictures folder; storage: file where we store scanned data
   */
  def main(args: Array[String]): Unit = {
    val (root, photoDir, storage) = args.toList match {
      case first::second::third::_ => (first, second, third)
      case first::second::_ => (first, second, DefaultListFile)
      case first::_ => (first, DefaultPhotoDir, DefaultListFile)
      case _ => (".", DefaultPhotoDir, DefaultListFile)
    }
//  val metadata = ImageMetadataReader.readMetadata(new File(imagePath))
    
    new File(storage).renameTo(new File(s"storage.${DateFormat.format(new Date)}.bak"))

    // data in our pictures folder
    val scannedPhotos: List[Record] = scan(new File(photoDir))
    val out0 = new PrintWriter(new FileWriter(storage))
    scannedPhotos foreach out0.println
    out0.close()

    // links in our pictures folder pointing outside
    val externalPhotoLinks = scannedPhotos collect {
      case link: SymbolicLink if !(link isInside photoDir) => link }
    
    // revert them all, so all images are in pictures folder
    for (link <- externalPhotoLinks) link.revert()

    // files with too late timestamps
    // that files timestamps are same as exif timestamps in images
    val filesToTouch = scannedPhotos collect { case fr: FileRecord if fr.fileTimestampDoneLater => fr }
    filesToTouch foreach (_.touch())
    
    // that files timestamps are same as exif timestamps in images
    val badFiles = scannedPhotos collect { case fr: FileRecord if fr.hasProblemWithTimestamp => fr }
    println("\n\nFiles With Problems\n")
    badFiles foreach println
//    require(badFiles.isEmpty, s"bad files: $badFiles")
    // the folder that may contain more pictures
    val rootFolder = new File(root)
    if (!rootFolder.isDirectory) fail(s"$root is not a directory")

    // links from outside into our pictures folder
    val linksFromOutside: List[Record] = scan(rootFolder) collect {
      case link: SymbolicLink => link
    }
    
    // merge these together, so there are no duplicates, and links are together with files
    // TODO: separate dedup operations and links organization
    val analyzed: List[FileGroup] = analyze(scannedPhotos ++ linksFromOutside)

    val out = new PrintWriter(new FileWriter(storage))
    analyzed foreach out.println
    out.close()

    val dataToProcess = analyzed filter (_.files.size > 1)
    println(dataToProcess)
    // TODO: make sure we know what we are doing
    //dataToProcess foreach (_.process())
  }

  /**
   * Groups all entries by their hash
   * TODO: separate dedup and links grouping
   * @param entries data we scan
   * @return a list of grouped data
   */
  def analyze(entries: List[Record]): List[FileGroup] = {
    
    val grouped: Map[String, List[Record]] = entries.groupBy(_.id)

    grouped.values.map(list => {
      val files = list flatMap (_.files) sortBy (_._2)
      FileGroup(files, list.head.size, list.head.hash)
    }).toList sorted
  }
}
