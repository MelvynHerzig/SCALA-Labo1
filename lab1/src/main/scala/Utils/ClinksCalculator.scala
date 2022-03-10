package Utils

import scala.annotation.tailrec

/**
  * Contains the function necessary to calculate the number of *clinks* when n people want to cheers.
  */
object ClinksCalculator:
  /**
    * Calculate the factorial of a given number
    * @param n the number to compute
    * @return n!
    */
  // TODO - Part 1 Step 1
  def factorial(n: Int): BigInt =
    @tailrec
    def iter(x: Int, result: BigInt): BigInt =
      if (x == 0 ) result
      else iter(x - 1, result * x)

    iter(n, 1)
  end factorial


  /**
    * Calculate the combination of two given numbers
    * @param n the first number
    * @param k the second number
    * @return n choose k
    */
  // TODO - Part 1 Step 1
  def calculateCombination(n: Int, k: Int): BigInt = factorial(n) / (factorial(k) * factorial(n-k))
end ClinksCalculator
