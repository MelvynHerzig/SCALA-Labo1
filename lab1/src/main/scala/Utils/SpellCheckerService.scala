package Utils

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Calculate the Levenstein distance between two words.
    *
    * @param s1 the first word
    * @param s2 the second word
    * @return an integer value, which indicates the Levenstein distance between "s1" and "s2"
    */
  def stringDistance(s1: String, s2: String): Int

  /**
    * Get the syntactically closest word in the dictionary from the given misspelled word, using the "stringDistance"
    * function. If the word is a number or a pseudonym, this function just returns it.
    *
    * @param misspelledWord the mispelled word to correct
    * @return the closest normalized word from "mispelledWord"
    */
  def getClosestWordInDictionary(misspelledWord: String): String
end SpellCheckerService

class SpellCheckerImpl(val dictionary: Map[String, String]) extends SpellCheckerService :
  // TODO - Part 1 Step 2
  def stringDistance(s1: String, s2: String): Int =
    def levenshteinDist(sa: String, sb: String, aLen: Int, bLen: Int): Int =
      (aLen, bLen) match
        case (-1, -1) => levenshteinDist(sa, sb, sa.length, sb.length)
        case (0, 0) => 0
        case (_, 0) => aLen
        case (0, _) => bLen
        case _ =>
          val l1 = levenshteinDist(sa, sb, aLen - 1, bLen)
          val l2 = levenshteinDist(sa, sb, aLen, bLen - 1)
          val l3 = levenshteinDist(sa, sb, aLen - 1, bLen - 1)
          val d = if (sa.slice(aLen - 1, aLen - 1 + 1) == sb.slice(bLen - 1, bLen - 1 + 1)) 0 else 1
          (l1+1).min(l2+1).min(l3+d)

    levenshteinDist(s1, s2, -1, -1)
  end stringDistance

  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String = ???
end SpellCheckerImpl

