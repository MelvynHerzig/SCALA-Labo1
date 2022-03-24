package Utils

import scala.annotation.tailrec

trait SpellCheckerService:
  /**
    * This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
    * we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
    */
  val dictionary: Map[String, String]

  /**
    * Function that checks if the given string is a number not necessarily integer nor positive.
    * @param s String to check if it is a number
    * @return True if the string is a number else False.
    */
  def isNumber(s: String): Boolean

  /**
    * Function that checks if the given string is a pseudonym (i.e: it starts with an _ )
    * @param s
    * @return
    */
  def isPseudonym(s: String): Boolean

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
  
  def isNumber(s: String): Boolean =
    s.matches("[+-]?\\d?\\d+")
  
  def isPseudonym(s: String): Boolean =
    s != "" && s.charAt(0) == '_'

  // TODO - Part 1 Step 2
  def stringDistance(s1: String, s2: String): Int =

    val upperCache = 0.to(s2.length).toArray
    var leftCache = 1
    var currentValue = -1

    @tailrec
    def sd (s1: String, s2: String, l1: Int, l2: Int): Int =

      if l2 == 0 then
        leftCache = l1 + 1

      if s1.charAt(l1) == s2.charAt(l2) then
        currentValue = upperCache(l2)
      else
        currentValue = leftCache.min(upperCache(l2).min(upperCache(l2 + 1))) + 1

      upperCache(l2) = leftCache
      leftCache = currentValue

      if l2 < s2.length - 1 then
        sd(s1, s2, l1, l2 + 1)
      else if l1 < s1.length - 1 then
        upperCache(l2+1) = currentValue
        sd(s1, s2, l1 + 1, 0)
      else
        currentValue
    end sd

    sd(s1, s2, 0, 0)
  end stringDistance

  // TODO - Part 1 Step 2
  def getClosestWordInDictionary(misspelledWord: String): String =
    if isNumber(misspelledWord) || isPseudonym(misspelledWord) then
      return misspelledWord

    var bestKeyFound = ("", Int.MaxValue)

    for (k,v) <- dictionary do
      val distance = stringDistance(k, misspelledWord)
      if distance < bestKeyFound._2 then
        bestKeyFound = (k, distance)
      else if distance == bestKeyFound._2 && k < bestKeyFound._1 then
        bestKeyFound = (k, distance)

    dictionary(bestKeyFound._1)

end SpellCheckerImpl

