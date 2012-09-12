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