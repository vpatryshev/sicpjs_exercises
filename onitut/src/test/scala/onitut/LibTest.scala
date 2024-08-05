package onitut

import org.specs2.mutable.Specification

import Lib._
import java.nio.file.{Files, Paths}
import java.nio.file.attribute.FileTime

class LibTest extends Specification {
  "Lib" should {
    "be able to calculate minimum of two values" in {
      min(Some(1), Some(2)) must beEqualTo(Option(1))
      min(Some(1), None) must beEqualTo(Option(1))
      min(None, Some(2)) must beEqualTo(Option(2))
      min(None, None) must beEqualTo(None)
    }

    "calculate year from timestamp" in {
      year(timestampOf(1989, 10, 9, 23, 0, 0)) must beEqualTo(1989)
      year(timestampOf(2024, 0, 1, 0, 0, 0)) must beEqualTo(2024)
      year(timestampOf(2024, 11, 31, 23, 59, 59)) must beEqualTo(2024)
      year(timestampOf(1806, 11, 31, 23, 59, 59)) must beEqualTo(1806)
    }
  }

  "Files" should {

    "be able to set positive timestamp" in {
      val path = Paths.get("/tmp/test1.jpg")
      try {Files.createFile(path)} catch {case x: Exception => ()}
      val timestamp = timestampOf(1989,10,9,23, 0, 0)
      setCreationTime(path, timestamp)

      creationTimeSeconds(path) must beEqualTo(Option(timestamp / 1000))
    }

    "be able to set negative timestamp" in {
      val path = Paths.get("/tmp/very_old.jpg")
      try {Files.delete(path)} catch {case x: Exception => ()}
      try {Files.createFile(path)} catch {case x: Exception => ()}
      val timestamp = -9223372036854L
      setCreationTime(path, timestamp)
      val actualTS = creationTimeSeconds(path)
      (actualTS
      aka s"actual $actualTS vs ${FileTime.fromMillis(timestamp)}") must beEqualTo(Option(timestamp / 1000))
    }

    "fail gracefully if file timestamp cannot be set" in {
      val path = Paths.get("/tmp")
      val originalTimestamp = creationTime(path)
      try {
        val timestamp = timestampOf(1996, 2, 1)
        setCreationTime(path, timestamp)
        fail("should not be able to update creation time of a directory")
      } catch {
        case x: Exception =>
          creationTime(path) must beEqualTo(originalTimestamp)
      }
    }

    "fail gracefully if file does not exist" in {
      val path = Paths.get(s"/tmp/does_not_exist_${System.currentTimeMillis}.jpg")
      try {
        val timestamp = timestampOf(1996, 2, 1)
        setCreationTime(path, timestamp)
        fail("should not be able to update creation time of a directory")
      } catch {
        case x: Exception => ok
      }
    }

  }
}
