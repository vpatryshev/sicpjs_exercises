package onitut

import onitut.images.ImageFiles._
import onitut.images._

import java.io.File
import java.nio.file.Paths
import java.text.SimpleDateFormat
import java.util.Date
import scala.language.postfixOps

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

  val HomeDir: File = new File(System.getProperty("user.home"))

  /**
   * This is the mac standard, pictures are in ~/Pictures
   */
  val DefaultPhotoDir: String = {
    new File(HomeDir, "Pictures").getAbsoluteFile.getCanonicalPath
  }
  
  val DefaultListFile: String = "filelist.out"

  def fixLink(photosByName1: Map[String, Seq[FileRecord]], photosByName2: Map[String, Seq[FileRecord]])(link: BadSymbolicLink): FileLink = {
    val path = link.path
    val mapOfCandidates = photosByName1.getOrElse(path.getFileName.toString.toLowerCase, Nil) map(record => (record.id, record)) toMap
    
    mapOfCandidates.values.toList match {
      case target::Nil =>
        link.fix(target)
      case Nil =>
        val foundMore: Seq[FileRecord] = photosByName2.getOrElse(path.getFileName.toString.toLowerCase, Nil)
        val mapOfCandidates2: Map[String, FileRecord] = foundMore map(record => (record.id, record)) toMap
        
        mapOfCandidates2.values.toList match {
          case target :: Nil => link.fix(target)
          case otherwise =>
            println("bad luck with " + (if (otherwise.isEmpty) "." else s": $otherwise"))
            link
        }
      case moreThanOne @ first::second::_ =>
        val notThumbnails = moreThanOne filterNot (_.isThumbnail)
        notThumbnails match {
          case one::_ => link.fix(one)
          case _ =>
            println(if (link.isThumbnail) s"Ignoring this bad thumbnail: $link" else
              s"Double choice for for $link:\n  ${first.toString}\n  ${second.toString}")
            link
            
        }
    }
  }

  def fixLinks(badLinksFromOutside: List[BadSymbolicLink],
    photosByName1: Map[String, Seq[FileRecord]],
    photosByName2: Map[String, Seq[FileRecord]]): List[FileLink] =
    badLinksFromOutside map fixLink(photosByName1, photosByName2)

  /**
   * 
   * @param args root: folder that we scan; photoDir: your pictures folder; storage: file where we store scanned data
   */
  def main(args: Array[String]): Unit = {
    val (root, photoDirString, storage) = args.toList match {
      case first::second::third::_ => (first, second, third)
      case first::second::_ => (first, second, DefaultListFile)
      case first::_ => (first, DefaultPhotoDir, DefaultListFile)
      case _ => (".", DefaultPhotoDir, DefaultListFile)
    }

    val photoDir = Paths.get(photoDirString)
    
    new File(storage).renameTo(new File(s"storage.${DateFormat.format(new Date)}.bak"))

    def badBackups = traverse(
      Option(_) filter(_.getName.toLowerCase.endsWith(".jpg.bak"))
    )(new File(root))

    for {
      file <- badBackups
      betterFile = new File(file.getPath.dropRight(4))
    } {
      if (betterFile.exists()) {
        println(s"wtf with $file?")
      } else {
        file.renameTo(betterFile)
        println(s"$file renamed")
      }
    }
    
    require(badBackups.isEmpty, "All bad backups were supposed to be fixed by now")

    // data in our pictures folder
    val scannedPhotos: List[FileOrLink] = scan(photoDir.toFile)

    dump(scannedPhotos, storage)

    revertExternalLinks(scannedPhotos, photoDir)

    // files with too late timestamps
    // that files timestamps are same as exif timestamps in images
    val filesToTouch = scannedPhotos collect { case fr: FileRecord if fr.fileTimestampDoneLater => fr }
    filesToTouch foreach (_.touch())
    
    // that files timestamps are same as exif timestamps in images
    val badFiles = scannedPhotos collect { case fr: FileRecord if fr.hasProblemWithTimestamp => fr }
    println("\n\nFiles With Problems\n")
    badFiles foreach println
    println("\n-------------------------------\n")

    val (filesByYear: Map[Int, List[FileRecord]], undatedPhotos: Seq[FileRecord]) =
      groupByYear(scannedPhotos, photoDir)

    //    require(badFiles.isEmpty, s"bad files: $badFiles")
    // the folder that may contain more pictures
    val rootFolder = new File(root)
    if (!rootFolder.isDirectory) fail(s"$root is not a directory")

    // links from outside into our pictures folder
    val linksFromOutside: List[FileLink] = scan(rootFolder) collect {
      case link: FileLink => link
    }

    val badLinksFromOutside: List[BadSymbolicLink] = linksFromOutside collect {
      case bl: BadSymbolicLink => bl
    }
    
    val datedPhotosByName: Map[String, List[FileRecord]] = {
      val collection: Iterable[(String, FileRecord)] =
        filesByYear.values.flatten.map(fr => fr.name -> fr)
      collection.groupBy(_._1).map{case (k, v) => k.toLowerCase -> v.map(_._2).toList}
    }

    val undatedPhotosByName: Map[String, Seq[FileRecord]] =
      undatedPhotos.groupBy(_.name.toLowerCase)

    val fixed = fixLinks(badLinksFromOutside, datedPhotosByName, undatedPhotosByName)
    val goodFixed = fixed collect { case link: SymbolicLink => link }
    val notFixed = fixed collect { case link: BadSymbolicLink => link}

    println(s"bad outside links: ${badLinksFromOutside.size}, fixed: ${goodFixed.size}, not fixed: ${notFixed.size}\n ${notFixed mkString "\n"}")
    
    println(s"Number of chained links: ${linksFromOutside.count(_ match {
      case SymbolicLink(_,_,n) => n > 1
      case _ => false
    })}")
    
    linksFromOutside foreach {
      case link: SymbolicLink => link.resolve()
      case _ => // do nothing
    }
    
    val goodLinksFromOutside: List[SymbolicLink] = linksFromOutside collect { case link: SymbolicLink => link }
    val goodLinksFromInside: List[SymbolicLink] = scannedPhotos collect { case link: SymbolicLink => link }
    
    val allGoodLinks: List[SymbolicLink] = goodLinksFromInside ++ goodLinksFromOutside
    
    val linksByTarget: Map[FileRecord, List[SymbolicLink]] = allGoodLinks groupBy(_.to)
    
    val wronglyPlaced: Map[Int, Seq[FileRecord]] = filesByYear.view.map {
      case (year, list) => year -> list.filter(_.year < year)
    } filter(_._2.nonEmpty) toMap
    
    println(s"\n\n${wronglyPlaced.values.map(_.size)sum} files are wrongly placed:\n")
//    println(wronglyPlaced.toList.sortBy(_._1) mkString "\n")
//    println("\n------------------\n\n")
    
    for {
      (_, files) <- wronglyPlaced
      file <- files
    } {
      file.moveToItsYear(photoDir, linksByTarget.getOrElse(file, Nil))
    }

    val duplicateNames: Map[String, List[FileRecord]] = datedPhotosByName filter(_._2.size > 1)
    println(duplicateNames mkString "\n")

    val sameFiles = duplicateNames.filter(kv => kv._2.tail.contains(kv._2.head))

    if (sameFiles.nonEmpty) {
      // merge these together, so there are no duplicates, and links are together with files
      // TODO: separate deduplicate operations and links organization
      val analyzed: List[FileGroup] = analyze(scannedPhotos ++ linksFromOutside)

      dump(analyzed, storage)

      val dataToProcess = analyzed filter (_.files.size > 1)
      println(dataToProcess)
      dataToProcess foreach (_.makeFirstFileLead())
    }    
  }
}
