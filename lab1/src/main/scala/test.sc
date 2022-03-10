def stringDistance(s1: String, s2: String): Int =
  def levenshteinDist(sa: String, sb: String, aLen: Int, bLen: Int, cont: (x: Int) => Int): Int =
    (aLen, bLen) match
      case (-1, -1) => levenshteinDist(sa, sb, sa.length, sb.length, cont)
      case (0, 0) => cont(0)
      case (_, 0) => cont(aLen)
      case (0, _) => cont(bLen)
      case _ =>
        levenshteinDist(sa, sb, aLen - 1, bLen, l1 =>
          levenshteinDist(sa, sb, aLen, bLen - 1, l2 =>
            levenshteinDist(sa, sb, aLen - 1, bLen - 1, l3 =>
              val d = if (sa.slice(aLen - 1, aLen - 1 + 1) == sb.slice(bLen - 1, bLen - 1 + 1)) 0 else 1
              cont((l1 + 1).min((l2 + 1).min(l3 + d)))
            )))

  levenshteinDist(s1, s2, -1, -1, x => x)
end stringDistance


stringDistance("lu", "la")
stringDistance("aur", "euzt")
stringDistance("auraa", "euzasdat")
