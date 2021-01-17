package onitut

import onitut.images.ImageFiles._
import onitut.images.{BadSymbolicLink, FileLink, FileOrLink, Record, SymbolicLink}

import java.io.{File, FileWriter, PrintWriter}
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

  def fixLink(photosByName1: Map[String, List[FileRecord]], photosByName2: Map[String, List[FileRecord]])(link: BadSymbolicLink): FileLink = {
    val path = link.path
    val mapOfCandidates = photosByName1.getOrElse(path.getFileName.toString.toLowerCase(), Nil) map(record => (record.id, record)) toMap
    
    mapOfCandidates.values.toList match {
      case target::Nil =>
        link.fix(target)
      case Nil =>
        val foundMore: Seq[FileRecord] = photosByName2.getOrElse(path.getFileName.toString.toLowerCase(), Nil)
        val mapOfCandidates2: Map[String, FileRecord] = foundMore map(record => (record.id, record)) toMap
        
        mapOfCandidates2.values.toList match {
          case target :: Nil => link.fix(target)
          case otherwise =>
            println(s"bad luck with $link: $otherwise")
            link
        }
      case moreThanOne @ first::second::_ =>
        val notThumbnails = moreThanOne filterNot (_.isThumbnail)
        notThumbnails match {
          case one::_ => link.fix(one)
          case _ =>
            println(if (link.isThumbnail) s"Ignoring this bad thumbnail: $link" else
              s"Double choice for for $link:\n  ${first.shortString}\n  ${second.shortString}")
            link
            
        }
    }
  }

  def fixLinks(badLinksFromOutside: List[BadSymbolicLink],
    photosByName1: Map[String, List[FileRecord]],
    photosByName2: Map[String, List[FileRecord]]): List[FileLink] =
    badLinksFromOutside map fixLink(photosByName1, photosByName2)

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

    def badBaks = fold(
      Option(_) filter(_.getName.toLowerCase.endsWith(".jpg.bak"))
    )(new File(root))

    for {
      file <- badBaks
      betterFile = new File(file.getPath.dropRight(4))
    } {
      if (betterFile.exists()) {
        println(s"wtf with $file?")
      } else {
        file.renameTo(betterFile)
        println(s"$file renamed")
      }
    }
    
    require(badBaks.isEmpty, "All bad baks were supposed to be fixed by now")

    // data in our pictures folder
    val scannedPhotos: List[FileOrLink] = scan(new File(photoDir))

    dump(scannedPhotos, storage)

    // links in our pictures folder pointing outside
    val externalPhotoLinks = scannedPhotos collect {
      case link: SymbolicLink if !(link isInside photoDir) => link }
    
    // revert them all, so all images are in pictures folder
//    for (link <- externalPhotoLinks) link.revert()

    // files with too late timestamps
    // that files timestamps are same as exif timestamps in images
    val filesToTouch = scannedPhotos collect { case fr: FileRecord if fr.fileTimestampDoneLater => fr }
    filesToTouch foreach (_.touch())
    
    // that files timestamps are same as exif timestamps in images
    val badFiles = scannedPhotos collect { case fr: FileRecord if fr.hasProblemWithTimestamp => fr }
    println("\n\nFiles With Problems\n")
    badFiles foreach println
    println("\n-------------------------------\n")

    val photosWithYearsMaybe = scannedPhotos.collect {
      case fr: FileRecord => (fr.folderYear(photoDir), fr)
    }
    
    val datedPhotos: List[(Int, FileRecord)] = photosWithYearsMaybe collect {
      case (Some(year: Int), fr) => year -> fr
    }
    
    val undatedPhotos: Seq[FileRecord] = photosWithYearsMaybe collect {
      case (None, fr) => fr
    }

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
    
    val filesByYear = (datedPhotos groupBy (_._1)).mapValues(_.map(_._2))
    
    val datedPhotosByName: Map[String, List[FileRecord]] = {
      val collection: Iterable[(String, FileRecord)] = filesByYear.values.flatten.map(fr => fr.file.getName -> fr)
      collection.groupBy(_._1).map{case (k, v) => k.toLowerCase -> v.map(_._2).toList}
    }

    val undatedPhotosByName: Map[String, List[FileRecord]] = {
      val collection: Iterable[(String, FileRecord)] = undatedPhotos.map(fr => fr.file.getName -> fr)
      collection.groupBy(_._1).map{case (k, v) => k.toLowerCase -> v.map(_._2).toList}
    }

    val fixed = fixLinks(badLinksFromOutside, datedPhotosByName, undatedPhotosByName)
    val goodOnes = fixed collect { case link: SymbolicLink => link }
    val badOnes = fixed collect { case link: BadSymbolicLink => link}

    println(s"bad outside links: ${badLinksFromOutside.size}, fixed: ${goodOnes.size}, not fixed: ${badOnes.size}\n ${badOnes mkString "\n"}")
    
    val duplicateNames: Map[String, Iterable[FileRecord]] = datedPhotosByName filter(_._2.size > 1)

    println(duplicateNames mkString "\n")
    
    val wronglyPlaced: Map[Int, Seq[FileRecord]] = filesByYear.view.map {
      case (year, list) => year -> list.filter(_.year < year)
    } filter(_._2.nonEmpty) toMap
    
    println("\n\nThese files are wrongly placed:\n")
    println(wronglyPlaced.toList.sortBy(_._1) mkString "\n")
    println("\n------------------\n\n")
    
    if (1 == 1) return
    
    
    // merge these together, so there are no duplicates, and links are together with files
    // TODO: separate dedup operations and links organization
    val analyzed: List[FileGroup] = analyze(scannedPhotos ++ linksFromOutside)

    dump(analyzed, storage)

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
  def analyze(entries: List[FileOrLink]): List[FileGroup] = {
    
    val grouped: Map[String, List[FileOrLink]] = entries.groupBy(_.id)

    grouped.values.map(list => {
      val files = list map (rec => (rec.path, rec.timestamp)) sortBy (_._2)
      FileGroup(files, list.head.size, list.head.id)
    }).toList sorted
  }
  
  def dump(data: List[Record], path: String): Unit = {
    val out = new PrintWriter(new FileWriter(path))
    data foreach out.println
    out.close()
  }
}
