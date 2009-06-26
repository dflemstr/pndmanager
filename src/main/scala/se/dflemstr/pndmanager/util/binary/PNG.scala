package se.dflemstr.pndmanager.util.binary

/** Data related to a PNG image */
object PNG {
  /** The binary pattern that indicates the start of a PNG file */
  final val BinaryStartPattern = Array(137, 80, 78, 71, 13, 10, 26, 10).map(_.toByte)
}
