package onitut.images

/**
 * Abstract record describing a file (image or link), or a group theirof, linked to one image file
 */
trait Record1 {
  /**
   * Identifies this record, taking the target id - or the file path if no target
   *
   * @return record id
   */
  def id: String
}

