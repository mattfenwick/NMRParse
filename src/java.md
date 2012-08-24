
[java specs](http://docs.oracle.com/javase/specs/jls/se7/jls7.pdf)

[nice, full grammar](http://www.cmis.brighton.ac.uk/staff/rnb/bosware/javaSyntax/rulesLinked.html#Literal)



tokens I used (didn't follow the spec exactly or completely):
    
    UnicodeInputCharacter:
            UnicodeEscape
            RawInputCharacter
    
    RawInputCharacter:
            any Unicode character
    
    LineTerminator:
            ASCII LF, also known as "newline"
            ASCII CR, also known as "return"
            ASCII CR then ASCII LF
    
    InputCharacter:
            UnicodeInputCharacter but not CR or LF
    
    Input:
            InputElements(?) Sub(?)
    
    InputElements:
            InputElement InputElements(?)
    
    InputElement:
            WhiteSpace
            Comment
            Token
    
    Token:
            Identifier
            Keyword
            Literal
            Separator
            Operator
    
    Sub:
            the ASCII SUB character, also known as "control-Z"
    
    WhiteSpace:
            the ASCII SP character, also known as "space"
            the ASCII HT character, also known as "horizontal tab"
            the ASCII FF character, also known as "form feed"
            LineTerminator
    
    Comment:
            EndOfLineComment
    
    EndOfLineComment:
            / / CharactersInLine(?) 
    
    CharactersInLine:
            InputCharacter CharactersInLine(?)
    
    Identifier:
            IdentifierChars but not a Keyword or BooleanLiteral or NullLiteral
    
    IdentifierChars:
            JavaLetter IdentifierRest(?)
            
    IdentifierRest:
            JavaLetterOrDigit IdentifierRest(?)
    
    JavaLetter:
            any Unicode character that is a Java letter (see below)
    
    JavaLetterOrDigit:
            any Unicode character that is a Java letter-or-digit (see below)
    
    Keyword: one of
            abstract    continue    for           new          switch
            assert      default     if            package      synchronized
            boolean     do          goto          private      this
            break       double      implements    protected    throw
            byte        else        import        public       throws
            case        enum        instanceof    return       transient
            catch       extends     int           short        try
            char        final       interface     static       void 
            class       finally     long          strictfp     volatile
            const       float       native        super        while                        
            
    Literal:
            IntegerLiteral
            FloatingPointLiteral
            BooleanLiteral
            CharacterLiteral
            StringLiteral
            NullLiteral
            
    IntegerLiteral:
            DecimalIntegerLiteral
    
    DecimalIntegerLiteral:
            DecimalNumeral IntegerTypeSuffix(?)
    
    IntegerTypeSuffix: one of
            l L
    
    DecimalNumeral:
            0
            NonZeroDigit Digits(?)
    
    Digits:
            Digit Digits(?)
    
    Digit:
            0
            NonZeroDigit
    
    NonZeroDigit: one of
            1 2 3 4 5 6 7 8 9
    
    FloatingPointLiteral:
            DecimalFloatingPointLiteral
    
    DecimalFloatingPointLiteral:
            Digits . Digits(?) ExponentPart(?) FloatTypeSuffix(?)
            . Digits ExponentPart(?) FloatTypeSuffix(?)      
            Digits ExponentPart FloatTypeSuffix(?)
            Digits ExponentPart(?) FloatTypeSuffix
    
    ExponentPart:
            ExponentIndicator SignedInteger
    
    ExponentIndicator: one of
            e E
    
    SignedInteger:
            Sign(?) Digits
    
    Sign: one of
            + -
    
    FloatTypeSuffix: one of
            f F d D
    
    BooleanLiteral: one of
            true false
    
    CharacterLiteral:
            ' SingleCharacter '
            ' EscapeSequence '
    
    SingleCharacter:
            InputCharacter but not ' or \
    
    StringLiteral:
            " StringCharacters(?) "
    
    StringCharacters:
            StringCharacter StringCharacters(?)
    
    StringCharacter:
            InputCharacter but not " or \
            EscapeSequence
    
    EscapeSequence:
            \ b                     /* \u0008: backspace BS                 */
            \ t                     /* \u0009: horizontal tab HT            */
            \ n                     /* \u000a: linefeed LF                  */
            \ f                     /* \u000c: form feed FF                 */
            \ r                     /* \u000d: carriage return CR           */
            \ "                     /* \u0022: double quote "               */
            \ '                     /* \u0027: single quote '               */
            \ \                     /* \u005c: backslash \                  */
    
    NullLiteral:
            null
    
    Separator: one of
            (       )       {       }       [       ]       ;       ,       .
    
    Operator: one of
            =       >       <       !       ~       ?       :
            ==      <=      >=      !=      &&      ||      ++      --
            +       -       *       /       &       |       ^       %       <<        >>        >>>
            +=      -=      *=      /=      &=      |=      ^=      %=      <<=       >>=       >>>=