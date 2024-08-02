package onitut

import org.specs2.mutable.Specification

import Lib._
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.FileTime

class FilesTest extends Specification {

  "Files" should {

    "be able to set positive timestamp" in {
      val path = Paths.get("/tmp/test1.jpg")
      try {Files.createFile(path)} catch {case x: Exception => ()}
      val timestamp = timestampOf(1989,10,9,23, 0, 0)
      touch(path, timestamp)

      creationTimeSeconds(path) must beEqualTo(Option(timestamp / 1000))
    }

    "be able to set negative timestamp" in {
      val path = Paths.get("/tmp/very_old.jpg")
      try {Files.delete(path)} catch {case x: Exception => ()}
      try {Files.createFile(path)} catch {case x: Exception => ()}
      val timestamp = -9223372036854L
      touch(path, timestamp)
      val actualTS = creationTimeSeconds(path)
      (actualTS
      aka s"actual $actualTS vs ${FileTime.fromMillis(timestamp)}") must beEqualTo(Option(timestamp / 1000))
    }
  }
}
