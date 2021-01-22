package onitut.images

import java.io.File
import java.util.Date

/**
 * Abstract record describing a file (image or link), or a group theirof, linked to one image file
 */
trait Record {
  /**
   * Identifies this record, taking the target id - or the file path if no target
   * @return record id
   */
  def id: String
//  def size: Long
}
