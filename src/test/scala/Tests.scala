import org.specs2.mutable.Specification

import Solutions._

object Tests extends Specification {

  "compress(ah) should return ah" >> {
    compress("ah") must_== "ah"
  }

  "compress(aaaah) should return a4h" >> {
    compress("aaaah") must_== "a4h"
  }

  "closestNumbersFunc(Array(6,2,4,10)) should return Array((2,4), (4,6))" >> {
    closestNumbersFunc(Array(6,2,4,10)) must_== Array((2,4), (4,6))
  }

  "kSub(3, Array(6,2,4,10)) should return 4" >> {
    kSub(3, Array(5, 1, 2, 3, 4, 1)) must_== 4L
  }

}
