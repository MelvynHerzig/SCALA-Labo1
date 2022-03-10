package Chat

enum Token:
  case // Terms
       BONJOUR, 
       JE,
       SVP,
       ASSOIFFE,
       AFFAME,
       // Actions
       ETRE,
       VOULOIR,
       // Logic Operators
       ET,
       OU,
       // Products
       PRODUCT,
       // Util
       PSEUDO,
       NUM,
       EOL,
       UNKNOWN,
       BAD
end Token
