package se.dflemstr.pndmanager

import _root_.org.specs._
import _root_.org.specs.runner._
import org.specs.util.DataTables
import se.dflemstr.pndmanager.util.BinaryTools

class BinaryToolsTest extends Runner(BinTools) with JUnit with Console

object BinTools extends Specification with DataTables {
  "The BinaryTools computeFailure tool" should {
    "produce correct failure patterns for given patterns" in {
      "pattern" | "failure"   |>
      "abcdef"  ! "000000"    |
      "4321"    ! "0000"      |
      "434343"  ! "001234"    |
      "444444"  ! "012345"    |
      "443443"  ! "010123"    | {
        (pat: String, fail: String) => { BinaryTools.computeFailure(pat).mkString must_== fail }
      }
    }
  }

  "The BinaryTools patternIndex tool should be able to find the index of basic patterns" in {
    "data"            | "pattern" | "index" |>
    "aaabc12333casjs" ! "abc123"  ! 2       |
    "aaabc12333casjs" ! "333c"    ! 7       |
    "33c33c333c3333"  ! "333c"    ! 6       |
    "aacbaskabcabckjs"! "abcabc"  ! 7       | {
      (data: String, pat: String, index: Int) => { BinaryTools.patternIndex(data, pat) must_== Some(index)}
    }
  }
}
