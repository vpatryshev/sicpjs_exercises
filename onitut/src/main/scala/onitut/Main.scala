package onitut

import onitut.images.ImageFiles._
import onitut.images._

import java.io.File
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date
import scala.Console.err
import scala.language.postfixOps
import onitut.Lib._

/**
 * Manages image files across the drive
 */
object Main:
  private val DateFormat: SimpleDateFormat =
    new SimpleDateFormat("yyyy-MM-dd")
  private val Extensions = ".*\\.jpg|jpeg"

  private val DefaultListFile: String = "filelist.out"

  private def fixLink(
    photosByName1: Map[String, Seq[FileRecord]],
    photosByName2: Map[String, Seq[FileRecord]]
  )(
    link: BadFileLink
  ): FileLink =
    val path = link.path
    val mapOfCandidates = photosByName1.getOrElse(path.getFileName.toString.toLowerCase, Nil) map(record => (record.id, record)) toMap
    
    mapOfCandidates.values.toList match
      case target::Nil =>
        link.fix(target)

      case Nil =>
        val foundMore: Seq[FileRecord] = photosByName2.getOrElse(path.getFileName.toString.toLowerCase, Nil)
        val mapOfCandidates2: Map[String, FileRecord] = foundMore map(record => (record.id, record)) toMap
        
        mapOfCandidates2.values.toList match
          case target :: Nil => link.fix(target)
          case otherwise => link.scold(
              "bad luck with " + (if (otherwise.isEmpty) "." else s": $otherwise"))

      case moreThanOne @ first::second::_ =>
        val notThumbnails = moreThanOne filterNot (_.isThumbnail)
        notThumbnails match
          case one::_ => link.fix(one)
          case _ =>
            log(if (link.isThumbnail) s"Ignoring this bad thumbnail: $link" else
              s"Double choice for for $link:\n  $first\n  $second")
            link

  private def fixLinks(badLinksFromOutside: List[BadFileLink],
                       photosByName1: Map[String, Seq[FileRecord]],
                       photosByName2: Map[String, Seq[FileRecord]]): List[FileLink] =
    badLinksFromOutside map fixLink(photosByName1, photosByName2)

  /**
   * 
   * @param args root: folder that we scan; photoDir: your pictures folder; storage: file where we store scanned data
   */
  def main(args: Array[String]): Unit =

    /**
     * This is the mac standard, pictures are in ~/Pictures
     */
    val DefaultPhotoDir: String =
      HomeDir.resolve("Pictures").toAbsolutePath.toString

    val (root, photoDirString, storage) = args.toList match
      case first::second::third::_ => (first, second, third)
      case first::second::_ => (first, second, DefaultListFile)
      case first::_ => (first, DefaultPhotoDir, DefaultListFile)
      case _ => (".", DefaultPhotoDir, DefaultListFile)

    val photoDir = Paths.get(photoDirString)
    
    new File(storage).renameTo(new File(s"storage.${DateFormat.format(new Date)}.bak"))

    lazy val badBackups = traverse(
      Option(_) filter(_.getName.toLowerCase.endsWith(".jpg.bak"))
    )(new File(root))

    for
      file <- badBackups
      betterFile = new File(file.getPath.dropRight(4))
    do
      if betterFile.exists() then
        err.println(s"wtf with $file?")
      else
        file.renameTo(betterFile)
        log(s"$file renamed")
    
    require(
      badBackups.isEmpty,
      "All bad backups were supposed to be fixed by now"
    )

    // data in our pictures folder
    val scannedPhotos: List[FileOrLink] = scan(photoDir.toFile)
      dump[FileOrLink](scannedPhotos, storage)

    revertExternalLinks(scannedPhotos, photoDir)

    // files with too late timestamps
    // that files timestamps are same as exif timestamps in images
    val filesToTouch = scannedPhotos collect:
      case fr: FileRecord if fr.fileTimestampDoneLater => fr

    filesToTouch foreach (_.touch())
    
    // that files timestamps are same as exif timestamps in images
    val badFiles = scannedPhotos collect:
      case fr: FileRecord if fr.hasProblemWithTimestamp => fr

    log("\n\nFiles With Problems\n")
    badFiles foreach log
    log("\n-------------------------------\n")

    val (filesByYear: Map[Int, List[FileRecord]], undatedPhotos: Seq[FileRecord]) =
      groupByYear(scannedPhotos, photoDir)

    //    require(badFiles.isEmpty, s"bad files: $badFiles")
    // the folder that may contain more pictures
    val rootFolder = new File(root)
    if (!rootFolder.isDirectory) fail(s"$root is not a directory")

    // links from outside into our pictures folder
    val linksFromOutside: List[FileLink] = scan(rootFolder) collect:
      case link: FileLink => link

    val badLinksFromOutside: List[BadFileLink] = linksFromOutside collect:
      case bl: BadFileLink => bl

    val datedPhotosByName: Map[String, List[FileRecord]] =
      val collection: Iterable[(String, FileRecord)] =
        filesByYear.values.flatten.map(fr => fr.name -> fr)
      collection.groupBy(_._1).map:
        case (k, v) => k.toLowerCase -> v.map(_._2).toList

    val undatedPhotosByName: Map[String, Seq[FileRecord]] =
      undatedPhotos.groupBy(_.name.toLowerCase)

    val fixed = fixLinks(badLinksFromOutside, datedPhotosByName, undatedPhotosByName)
    val goodFixed = fixed collect { case link: SymbolicLink => link }

    val notFixed = fixed collect { case link: BadFileLink => link}

    log(s"bad outside links: ${badLinksFromOutside.size}, fixed: ${goodFixed.size}, not fixed: ${notFixed.size}\n ${notFixed mkString "\n"}")
    
    log(s"Number of chained links: ${linksFromOutside.count(
      _ match
        case SymbolicLink(_,_,n) => n > 1
        case _ => false
    )}")
    
    linksFromOutside foreach:
      case link: SymbolicLink => link.resolve()

    val goodLinksFromOutside: List[SymbolicLink] = linksFromOutside collect { case link: SymbolicLink => link }
    val goodLinksFromInside: List[SymbolicLink] = scannedPhotos collect { case link: SymbolicLink => link }
    
    val allGoodLinks: List[SymbolicLink] = goodLinksFromInside ++ goodLinksFromOutside
    
    val linksByTarget: Map[FileRecord, List[SymbolicLink]] = allGoodLinks groupBy(_.to)
    
    val wronglyPlaced: Map[Int, Seq[FileRecord]] = filesByYear.view.collect {
      case (groupYear, list) if list.nonEmpty =>
        groupYear -> list.filter(_.year < groupYear)
    } toMap
    
    log(s"\n\n${wronglyPlaced.values.map(_.size)sum} files are wrongly placed:\n")
//    log(wronglyPlaced.toList.sortBy(_._1) mkString "\n")
//    log("\n------------------\n\n")
    
    for
      (_, files) <- wronglyPlaced
      file <- files
    do
      file.moveToItsYear(photoDir, linksByTarget.getOrElse(file, Nil))

    val duplicateNames: Map[String, List[FileRecord]] = datedPhotosByName filter(_._2.size > 1)
    println(duplicateNames mkString "\n")

    val sameFiles = duplicateNames.filter(kv => kv._2.tail.contains(kv._2.head))

    if (sameFiles.nonEmpty)
      // merge these together, so there are no duplicates, and links are together with files
      // TODO: separate deduplicate operations and links organization
      val analyzed: List[FileGroup] = analyze(scannedPhotos ++ linksFromOutside)

      dump(analyzed, storage)

      val dataToProcess = analyzed filter (_.files.size > 1)
      println(dataToProcess)
      dataToProcess foreach (_.makeFirstFileLead())
