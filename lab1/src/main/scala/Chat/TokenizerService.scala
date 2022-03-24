package Chat

import Chat.Token.*
import Utils.SpellCheckerService

import scala.language.postfixOps

class TokenizerService(spellCheckerSvc: SpellCheckerService):


  def getCorrespondingToken(word : String) : Token =
    word match {
      case "bonjour" => Token.BONJOUR
      case "je"=> Token.JE
      case "etre" => Token.ETRE
      case "vouloir" => Token.VOULOIR
      case "assoiffe" => Token.ASSOIFFE
      case "affame" => Token.AFFAME
      case "biere" => Token.PRODUCT
      case "croissant" => Token.PRODUCT
      case "et" => Token.ET
      case "ou" => Token.OU
      case "svp" => Token.SVP
      case a if spellCheckerSvc.isPseudonym(a) => Token.PSEUDO
      case a if spellCheckerSvc.isNumber(a) => Token.NUM
      case _ => Token.UNKNOWN
    }
  end getCorrespondingToken


  /**
    * Separate the user's input into tokens
    * @param input The user's input
    * @return A Tokenizer which allows iteration over the tokens of the input
    */
  def tokenize(input: String): Tokenized =
    val words = input.replaceAll("[.,!?*]", "").replaceAll("[']", " ").split(" ")

    var tokens : Array[(String, Token)] = Array()

    for word <- words do
      val realWord : String = spellCheckerSvc.getClosestWordInDictionary(word)
      print("word: " + word + " realWord " + realWord)
      tokens = tokens :+ (realWord, getCorrespondingToken(realWord))

    TokenizedImpl(tokens)
  end tokenize


end TokenizerService
