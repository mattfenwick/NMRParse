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
    | AEVAInit [ASTNode]
    
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
    | AMethodDecl [ASTNode] (Maybe ASTNode) ASTNode ASTNode [ASTNode] ASTNode
    
    -- field declaration
    --   type, identifiers + optional assignments
    | AFieldDecl ASTNode [ASTNode]
    
    -- variable declaration
    --   identifier, # array nesting level, optional initialization
    | AVarDecl ASTNode Int (Maybe ASTNode)
    
    -- constructor declaration
    --   type parameters, identifier, parameters, throws, required block
    | AConsDecl [ASTNode] ASTNode ASTNode [ASTNode] ASTNode
    
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
    | AIMethodDecl [ASTNode] (Maybe ASTNode) ASTNode ASTNode [ASTNode]
    
    -- intf. field declaration
    --   type, identifiers + assignments
    | AIFieldDecl ASTNode [ASTNode]
    
    -- intf. constant declaration
    --   identifier, # array nesting levels, initialization
    | AIConstDecl ASTNode Int ASTNode



-- ---- Section 8

    -- formal parameter
    --   variable modifiers, type, identifier
    | AFParam [ASTNode] ASTNode ASTNode
    
    -- formal parameters
    --   normal parameters, optional var-args parameter
    | AFParams [ASTNode] (Maybe ASTNode)
    
    -- array initializer
    --   element values
    | AArrayInit [ASTNode]



-- ---- Section 9

    -- block
    --   block statements
    | ABlock [ASTNode]
    
    -- local variable declaration statement
    --   variable modifiers, type, variable declarations
    | ALVDStmnt [ASTNode] ASTNode [ASTNode]
    
    -- label
    --   label name, statement
    | ALabel ASTNode ASTNode
    
    -- switch block statement group
    --   switch labels (should be at least 1), block statements
    | ASwitchSGs [ASTNode] [ASTNode]
    
    -- case label
    --   the expression (Nothing means 'default')
    | ACase (Maybe ASTNode)
    
    -- switch statement
    --   switch value, statement groups
    | ASwitch ASTNode [ASTNode]
    
    -- if statement
    --   expression, statement, optional else-statement
    | AIf ASTNode ASTNode (Maybe ASTNode)
    
    -- assert statement
    --   boolean expression, optional message expression
    | AAssert ASTNode (Maybe ASTNode)
    
    -- while loop
    --   boolean expression, body
    | AWhile ASTNode ASTNode
    
    -- do loop
    --   body, boolean expression
    | ADoLoop ASTNode ASTNode
    
    -- for loop
    --   ???
    
    -- break statement
    --   optional identifier
    | ABreak (Maybe ASTNode)
    
    -- continue statement
    --   optional identifier
    | AContinue (Maybe ASTNode)
    
    -- return statement
    --   optional expression
    | AReturn (Maybe ASTNode)
    
    -- throw statement
    --   expression
    | AThrow ASTNode
    
    -- synchronized thing
    --   expression, block
    | ASynch ASTNode ASTNode
    
    -- catch clause
    --   variable modifiers, type, identifier, block
    | ACatch [ASTNode] ASTNode ASTNode ASTNode
    
    -- try block
    --   block, catch clauses, optional finally
    | ATry ASTNode [ASTNode] (Maybe ASTNode)
    
    -- resource
    --   variable modifiers, reference type, identifier, # array levels, expression
    | AResource [ASTNode] ASTNode ASTNode Int ASTNode
    
    -- try with resources
    --   resources, block, catch clauses, optional finally
    | ATryResource [ASTNode] ASTNode [ASTNode] (Maybe ASTNode)


  deriving (Show, Eq, Ord)