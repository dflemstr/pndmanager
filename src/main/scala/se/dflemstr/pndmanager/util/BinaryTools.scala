package se.dflemstr.pndmanager.util

/**
 * Contains various tools that are useful when parsing binaries of various types
 */
object BinaryTools {

  private def nextIndex[A](oldIndex: Int, value: A, pattern: Seq[A]): Int =
    if(oldIndex > 0 && pattern(oldIndex) != value)
      nextIndex(oldIndex - 1, value, pattern)
    else
      oldIndex
      
  /**
   * Use the Knuth-Morris-Pratt Algorithm to locate a pattern within a large piece of data
   */
  def patternIndex[A](data: Seq[A], pattern: Seq[A]): Option[Int] =
    patternIndex[A](data, pattern, computeFailure(pattern))

  /**
   * Use the Knuth-Morris-Pratt Algorithm to locate a pattern within a large piece of data
   */
  def patternIndex[A](data: Seq[A], pattern: Seq[A], failure: Seq[Int]): Option[Int] = {
    //TODO: use more descriptive variable names
    def scanAt(start: Int, index: Int): Option[Int] =
      if(start == data.length)
        None
      else {
        val i = nextIndex(index, data(start), pattern)
        val newIndex =
          if (pattern(i) == data(start))
            i + 1
          else
            i
            
        if (newIndex == pattern.length)
          Some(start - pattern.length + 1)
        else
          scanAt(start + 1, newIndex)
      }

    if (data.length == 0)
      None
    else
      scanAt(0, 0)
  }

  /**
   * Computes the "failure pattern" to be used with patternIndex
   */
  def computeFailure[A](pattern: Seq[A]): Seq[Int] = {
    def parse(start: Int, index: Int, failure: List[Int]): List[Int] =
      if(start == pattern.length)
        failure
      else {
        val i = nextIndex(index, pattern(start), pattern)
        if(pattern(i) == pattern(start))
          parse(start + 1, i + 1, (i + 1) :: failure)
        else
          parse(start + 1, i, i :: failure)
      }

    parse(1, 0, List(0)).reverse.toArray
  }
}
