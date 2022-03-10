import scala.annotation.tailrec

//def stringDistance(s1: String, s2: String): Int =
//  @tailrec
//  def levenshteinDist(sa: String, sb: String, aLen: Int, bLen: Int, cont: (x: Int) => Int): Int =
//    (aLen, bLen) match
//      case (-1, -1) => levenshteinDist(sa, sb, sa.length, sb.length, cont)
//      case (0, 0) => cont(0)
//      case (_, 0) => cont(aLen)
//      case (0, _) => cont(bLen)
//      case _ =>
//        levenshteinDist(sa, sb, aLen - 1, bLen, l1 =>
//          levenshteinDist(sa, sb, aLen, bLen - 1, l2 =>
//            levenshteinDist(sa, sb, aLen - 1, bLen - 1, l3 =>
//              val d = if (sa.slice(aLen - 1, aLen - 1 + 1) == sb.slice(bLen - 1, bLen - 1 + 1)) 0 else 1
//              cont((l1 + 1).min((l2 + 1).min(l3 + d)))
//            )))
//
//  levenshteinDist(s1, s2, -1, -1, x => x)
//end stringDistance
//def stringDistance(s1: String, s2: String): Int =
//  def levenshteinDist(sa: String, sb: String, aLen: Int, bLen: Int): Int =
//    (aLen, bLen) match
//      case (-1, -1) => levenshteinDist(sa, sb, sa.length, sb.length)
//      case (0, 0) => 0
//      case (_, 0) => aLen
//      case (0, _) => bLen
//      case _ =>
//        val l1 = levenshteinDist(sa, sb, aLen - 1, bLen)
//        val l2 = levenshteinDist(sa, sb, aLen, bLen - 1)
//        val l3 = levenshteinDist(sa, sb, aLen - 1, bLen - 1)
//        val d = if (sa.slice(aLen - 1, aLen ) == sb.slice(bLen - 1, bLen )) 0 else 1
//        (l1+1).min(l2+1).min(l3+d)
//
//  levenshteinDist(s1, s2, -1, -1)
//end stringDistance

//def stringDistance(s1: String, s2: String): Int = {
//
//  // Memoization
//  val memo = scala.collection.mutable.Map[(List[Char],List[Char]),Int]()
//
//  // Triple min
//  def min(a:Int, b:Int, c:Int) = Math.min( Math.min( a, b ), c)
//
//  // Recursive function
//  def sd(s1: List[Char], s2: List[Char]): Int = {
//    if (memo.contains((s1,s2)) == false)
//      memo((s1,s2)) = (s1, s2) match {
//        case (_, Nil) => s1.length
//        case (Nil, _) => s2.length
//        case (c1::t1, c2::t2)  => min( sd(t1,s2) + 1, sd(s1,t2) + 1,
//          sd(t1,t2) + (if (c1==c2) 0 else 1) )
//      }
//    memo((s1,s2))
//  }
//
//  sd( s1.toList, s2.toList )
//}

def stringDistance(s1: String, s2: String): Int =

  val upperCache = 0.to(s2.length).toArray
  var leftCache = 1
  var currentValue = -1

  @tailrec
  def sd (s1: String, s2: String, l1: Int, l2: Int): Int =

    if(l2 == 0) then
      leftCache = l1 + 1

    if (s1.charAt(l1) == s2.charAt(l2))
      currentValue = upperCache(l2)
    else
      currentValue = leftCache.min(upperCache(l2).min(upperCache(l2 + 1))) + 1

    upperCache(l2) = leftCache
    leftCache = currentValue

    if(l2 < s2.length - 1) then
      sd(s1, s2, l1, l2 + 1)
    else if (l1 < s1.length - 1) then
      upperCache(l2+1) = currentValue
      sd(s1, s2, l1 + 1, 0)
    else
      currentValue
  end sd

  sd(s1, s2, 0, 0)
end stringDistance

stringDistance("aur", "euzt")
stringDistance("auraa", "euzasdat")
