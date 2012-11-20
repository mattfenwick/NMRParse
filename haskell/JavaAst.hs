module JavaAst (

    ASTNode(..)

) where


data ASTNode =

-- ---- Section 1

    -- identifier
      AIdent    String
    
    -- qualified identifier
    --   identifiers
    | AQIdent   [ASTNode] 

    -- qualified identifier list
    --   qualified identifiers
    | AQIdentList [ASTNode]        

    

-- ---- Section 2

    -- annotation type declaration
    --   identifier, body
    | AAnnoDecl ASTNode ASTNode

    -- normal interface declaration
    --   identifier, type parameters, extends, body
    | AIntfDecl ASTNode [ASTNode] [ASTNode] ASTNode      

    -- enum declaration
    --   identifier, implements, body
    | AEnumDecl ASTNode [ASTNode] ASTNode            

    -- class declaration
    --   identifier, type parameters, extends, implements, body
    | AClassDecl ASTNode [ASTNode] (Maybe ASTNode) [ASTNode] ASTNode

    -- class or enum declaration
    --   modifiers, declaration
    | ACOrIDecl [ASTNode] ASTNode    

    -- import declaration
    --   is static, qualfied identifier, is *
    | AImport Bool [ASTNode] Bool            

    -- package declaration
    --   annotations, qualified identifier
    | APackDecl [ASTNode] ASTNode                    

    -- compilation unit
    --   package declaration, imports, types
    | ACompUnit (Maybe ASTNode) [ASTNode] [ASTNode]



-- ---- Section 3

    -- basic type (short, double, etc.)
    --   type name
    | ABType String

    -- type argument with optional extends/super clause
    --   extends/super, reference type
    | ATypeArg (Maybe (String, ASTNode))
    
    -- reference type
    --   parameterized types
    | ARefType [ASTNode]
    
    -- a type
    --   base type, is array
    | AType ASTNode Bool                             

    -- parameterized type
    --   identifier, type args
    | AParamType ASTNode [ASTNode]


    
-- ---- Section 4

    -- type bound
    --   types
    | ABound [ASTNode]  
    
    -- type parameter
    --   identifier, bounds
    | ATypeParam ASTNode ASTNode
    
    
    
-- ---- Section 5

    -- element value array initializer
    --   element values
    | AArrayInit [ASTNode]
    
    -- element value pair
    --   identifier, element value
    | APair ASTNode ASTNode
    
    -- annotations
    --   qualfied identifier, annotation elements
    | AAnno ASTNode [ASTNode]
    
    -- keyword modifier
    --   the keyword
    | AModifier String


  deriving (Show, Eq, Ord)