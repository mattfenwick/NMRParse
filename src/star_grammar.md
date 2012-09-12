A grammar for the NMR-Star file format, based on the published specs but not identical:

    NMRStar  ::=  Data
    
    Data     ::=  DataOpen  Save(+)
    
    Save     ::=  SaveOpen  ( Datum  |  Loop )(+)  SaveClose
    
    Datum    ::=  Identifier  Value
    
    Loop     ::=  LoopOpen  Identifier(+)  Value(+)  LoopClose


Notes:

 - lack of Global blocks
 
 - Datums not allowed in Data blocks
 
 - only one Data block allowed per file
 
 - Datums and Loops allowed in any order in Save blocks
 
 - only 1-d loops allowed
 
 - Loops must be explicitly closed
 
 
Tokens:

    newline    ::=  ( '\n'  |  '\r'  |  '\f' )(+)
    
    comment    ::=  '#'  (not newline)(*)
    
    whitespace ::=  ( ' '  |  '\t'  |  '\v' )(+)
    
    dataOpen   ::=  "data_"  (not ( whitespace  |  newline ))(+)
    
    saveOpen   ::=  "save_"  (not ( whitespace  |  newline ))(+)
    
    saveClose  ::=  "save_"
    
    loopOpen   ::=  "loop_"
    
    loopClose  ::=  "stop_"
    
    identifier ::=  '_'  (not ( whitespace  |  newline ))(+)
       
    value = pany [sqstring, dqstring, scstring, sbstring, unquoted]
      where
        sqstring   ::=  '\''  (not '\'')(*)  '\''
    
        dqstring   ::=  '"'  (not '"')(*)  '"'
    
        scstring   ::=  ';'  (not ';')(*)  ';'
    
        sbstring   ::=  '['  (not ( '['  |  ']' ))(*)  ']'
    
        unquoted   ::=  (not control)  (not ( whitespace | newline ) )(*)
          where control = ( '"' | '#' | '\'' | '[' | ']' | '_' | whitespace | newline )

Token precedence:  
  Is this necessary?  we want to get the longest matches
    dataOpen, saveOpen, saveClose, loopOpen, loopClose have to come before unquoted values
    saveOpen has to come before saveClose

  - dataOpen
  - saveOpen
  - saveClose
  - loopOpen
  - loopClose
  - value
  - whitespace
  - newline
  - comment
  - identifier
