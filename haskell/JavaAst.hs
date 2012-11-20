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
    
    -- an empty ... something
    | AEmpty



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
    --   base type, # of levels of array nesting
    | AType ASTNode Int                       

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
    --   qualified identifier, annotation elements
    | AAnno ASTNode [ASTNode]
    
    -- keyword modifier
    --   the keyword
    | AModifier String



-- ---- Section 6

    -- class body
    --   class body declarations
    | AClassBody [ASTNode]
    
    -- member declaration
    --   modifiers, method/field/constructor/class/interface
    | AMemberDecl [ASTNode] ASTNode
    
    -- method declaration
    --   type parameters, void or type, identifier, parameters, throws, block
    | AMethodDecl [ASTNode] (Maybe ASTNode) ASTNode [ASTNode] [ASTNode] ASTNode
    
    -- field declaration
    --   type, identifiers + optional assignments
    | AFieldDecl ASTNode [ASTNode]
    
    -- variable declaration
    --   identifier, # array nesting level, optional initialization
    | AVarDecl ASTNode Int (Maybe ASTNode)
    
    -- constructor declaration
    --   type parameters, identifier, parameters, throws, required block
    | AConsDecl [ASTNode] ASTNode [ASTNode] [ASTNode] ASTNode
    
    -- class body block
    --   is static, block
    | AClassBlock Bool ASTNode



-- ---- Section 7

    -- interface body
    --   intf. body declarations
    | AIntfBody [ASTNode]
    
    -- intf. member declaration
    --   modifiers, method/field/class/interface
    | AIMemberDecl [ASTNode] ASTNode
    
    -- intf. method declaration
    --   type parameters, void or type, identifier, parameters, throws
    | AIMethodDecl [ASTNode] (Maybe ASTNode) ASTNode [ASTNode] [ASTNode]
    
    -- intf. field declaration
    --   type, identifiers + assignments
    | AIFieldDecl ASTNode [ASTNode]
    
    -- intf. constant declaration
    --   identifier, # array nesting levels, initialization
    | AIConstDecl ASTNode Int ASTNode



-- ---- Section ??

    -- block
    --   block statements
    | ABlock [ASTNode]




  deriving (Show, Eq, Ord)