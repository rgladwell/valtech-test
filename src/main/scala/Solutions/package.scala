package object Solutions {

  def compress(string: String): String = {

    def tokenize(string: String): Seq[String] = string.headOption match {

      case Some(head) => {
        val token = string.takeWhile{ _ == head }.mkString
        token +: tokenize(string.drop(token.length).mkString)
      }


      case _ => Nil

    }

    val encodedTokens =
      for {
        token <- tokenize(string)
      } yield {
        if(token.length != 1) s"${token.head}${token.length}"
        else token
      }

    encodedTokens.mkString
  }

  def closestNumbersFunc(numbers: Array[Int]): Array[(Int, Int)] = {
    def diff(tuple: (Int, Int)) = tuple match { case (i, j) => scala.math.abs(j - i) }

    val combinations = numbers.combinations(2).toArray.map{ case Array(i, j) => if(i < j) (i, j) else (j, i) }
    val minDiff = combinations.map(diff).min
    combinations.filter{ diff(_) == minDiff }.sortBy(_._1)
  }

  def closestNumbers(numbers: Array[Int]) =
    for {
      (left, right) <- closestNumbersFunc(numbers)
    } yield println(s"$left $right")

  def kSub(k: Int, nums: Array[Int]): Long = {
    nums.permutations.filter{ subsequence => println(s"(${subsequence.sum} % $k) = ${(subsequence.sum % k)}"); (subsequence.sum % k) == 0 }.size
  }

}
