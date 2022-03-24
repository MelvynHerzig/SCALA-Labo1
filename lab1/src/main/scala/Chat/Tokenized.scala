package Chat

import Chat.Token.*
import Utils.SpellCheckerService

trait Tokenized:
  /**
    * Get the next token of the user input, or EOL if there is no more token.
    * @return a tuple that contains the string value of the current token, and the identifier of the token
    */
  def nextToken(): (String, Token)

class TokenizedImpl(val tokens: Array[(String, Token)]) extends Tokenized:

  /**
    * Index that indicates which is next token index to return when nextToken is called.
    */
  private var index = -1;

  def nextToken(): (String, Token) =

    index += 1
    
    // If not every token have been returned
    if index < tokens.length then
      tokens(index)
    else
      ("EOL", Token.EOL)
  end nextToken

end TokenizedImpl
