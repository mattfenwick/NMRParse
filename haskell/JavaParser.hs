module JavaParser (

) where

import Java
import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import JavaTokens
import JavaAst



-- ------------------
-- the 'real', fancy parsers
-- they'll be working on [Token]


sepBy1Fst :: Parser t a -> Parser t b -> Parser t [a]
sepBy1Fst p = fmap fst . sepBy1 p


opt :: a -> Parser t a -> Parser t a
opt x p = p <|> pure x


key :: Keyword -> Parser Token Token
key = literal . Keyword


sep :: Separator -> Parser Token Token
sep = literal . Separator


op :: Operator -> Parser Token Token
op = literal . Operator



-- Section 1: identifiers

identifier :: Parser Token ASTNode
identifier = 
    getOne >>= \t -> case t of
                     (Identifier i) -> pure (AIdent i)
                     _              -> empty


qualifiedIdentifier :: Parser Token ASTNode
qualifiedIdentifier = 
    pure AQIdent <*>
    sepBy1Fst identifier (sep Period)


qualifiedIdentifierList :: Parser Token [ASTNode]
qualifiedIdentifierList = 
    sepBy1Fst qualifiedIdentifier (sep Comma)



-- Section 2:  imports, declarations

-- TODO: can this parse a typelist of zero
-- types ... or are there some delimiters
-- to watch out for?
extsClause :: Parser Token [ASTNode]
extsClause = 
    key Kextends  *>
    typeList


impsClause :: Parser Token [ASTNode]
impsClause = 
    key Kimplements  *> 
    typeList


annotationTypeDeclaration :: Parser Token ASTNode
annotationTypeDeclaration = 
    pure AAnnoDecl    <*>
    (literal AtSign    *>
     key Kinterface    *>
     identifier)      <*>
     annotationTypeBody


annotationTypeBody = error "TODO!"


normalInterfaceDeclaration :: Parser Token ASTNode
normalInterfaceDeclaration =
    pure AIntfDecl          <*>
    (key Kinterface          *> 
     identifier)            <*>
     opt [] typeParameters  <*>
     opt [] extsClause      <*>
     interfaceBody


enumDeclaration :: Parser Token ASTNode
enumDeclaration = 
    pure AEnumDecl      <*>
    (key Kenum           *>
     identifier)        <*>
     opt [] impsClause  <*>
     enumBody


enumBody = error "TODO"


normalClassDeclaration :: Parser Token ASTNode
normalClassDeclaration = 
    pure AClassDecl         <*>
    (key Kclass              *> 
     identifier)            <*>
     opt [] typeParameters  <*>
     extendsOpt             <*> 
     opt [] impsClause      <*>
     classBody
  where
    extendsOpt = optional (key Kextends *> jtype)


interfaceDeclaration :: Parser Token ASTNode
interfaceDeclaration = 
    normalInterfaceDeclaration    <|>
    annotationTypeDeclaration


classDeclaration :: Parser Token ASTNode
classDeclaration = 
    normalClassDeclaration    <|> 
    enumDeclaration


classOrInterfaceDeclaration :: Parser Token ASTNode
classOrInterfaceDeclaration = 
    pure ACOrIDecl         <*> 
    many modifier          <*> 
    (classDeclaration <|> interfaceDeclaration)


emptyThing :: Parser Token ASTNode
emptyThing = 
    sep Semicolon *> 
    pure AEmpty


-- I think the empty ; rule is just to allow lots of
-- trailing semicolons ... it can probably be stuck
-- on to compilationUnit instead
typeDeclaration :: Parser Token ASTNode
typeDeclaration = 
    classOrInterfaceDeclaration         <|> 
    emptyThing


importDeclaration :: Parser Token ASTNode
importDeclaration = 
    pure AImport                            <*>
    (key Kimport                             *>
     opt False (key Kstatic *> pure True))  <*>
     sepBy1Fst identifier (sep Period)      <*>
     opt False (op Times *> pure True)      <*
     sep Semicolon


packageDecl :: Parser Token ASTNode
packageDecl = 
    pure APackDecl            <*>
    many annotation           <*>
    (key Kpackage              *>
     qualifiedIdentifier      <*
     sep Semicolon)


compilationUnit :: Parser Token ASTNode
compilationUnit = 
    pure ACompUnit           <*>
    optional packageDecl     <*>
    many importDeclaration   <*>
    many typeDeclaration
    



-- Section 3:  types


basicType :: Parser Token ASTNode
basicType = mconcat $ map key2 [Kbyte, Kshort, Kchar, Kint, Klong, Kfloat, Kdouble, Kboolean]
  where
    key2 k = 
        literal (Keyword k)            *> 
        pure (ABType $ tail $ show k)


typeArgument :: Parser Token ASTNode
typeArgument = 
    referenceType           <|>
    fmap ATypeArg bounded
  where
    bounded =
        op QuestionMark >>
        optional (pure (,)        <*>
                  extendsOrSuper  <*>
                  referenceType)
    extendsOrSuper = 
        fmap (tail . show) (key Kextends <|> key Ksuper) -- TODO:  tail . show doesn't work


typeArguments :: Parser Token [ASTNode]
typeArguments =
    op LessThan                          *> 
    sepBy1Fst typeArgument (sep Comma)  <* 
    op GreaterThan


referenceType :: Parser Token ASTNode
referenceType =
    pure ARefType     <*>
    sepBy1Fst paramType (sep Period)
  where
    paramType = 
        pure AParamType       <*>
        identifier            <*> 
        opt [] typeArguments


array :: Parser Token ()
array = 
    sep OpenSquare   *> 
    sep CloseSquare  *>
    pure ()


jtype :: Parser Token ASTNode
jtype =
    pure AType                     <*>
    (basicType <|> referenceType)  <*>
    (fmap length $ many array)


-- Section 4: generics

bound :: Parser Token ASTNode
bound = 
    pure ABound     <*>
    sepBy1Fst referenceType (op And)


typeParameter :: Parser Token ASTNode
typeParameter = 
    pure ATypeParam   <*>
    identifier        <*> 
    (key Kextends      *> 
     bound)


typeParameters :: Parser Token [ASTNode]
typeParameters = 
    op LessThan                           *> 
    sepBy1Fst typeParameter (sep Comma)  <* 
    op GreaterThan


nonWildcardTypeArguments :: Parser Token [ASTNode]
nonWildcardTypeArguments = 
    op LessThan      *> 
    typeList        <* 
    op GreaterThan


typeList :: Parser Token [ASTNode]
typeList = sepBy1Fst referenceType (sep Comma)


diamond :: Parser Token ()
diamond = 
    pure ()         <* 
    op LessThan     <* 
    op GreaterThan


typeArgumentsOrDiamond :: Parser Token [ASTNode]
typeArgumentsOrDiamond = 
    (diamond *> pure [])       <|> 
    typeArguments


nonWildcardTypeArgumentsOrDiamond :: Parser Token [ASTNode]
nonWildcardTypeArgumentsOrDiamond = 
    (diamond *> pure [])       <|> 
    nonWildcardTypeArguments




-- Section 5:

elementValues :: Parser Token [ASTNode]
elementValues = sepBy1Fst elementValue (sep Comma)


{- this is very strange;  here are some examples:
		@SuppressWarnings({ })
		ArrayList q1 = new ArrayList();
		@SuppressWarnings({ ,})
		ArrayList q2 = new ArrayList();
		@SuppressWarnings(value = { })
		ArrayList q3 = new ArrayList();
		@SuppressWarnings(value = {, })
		ArrayList q4 = new ArrayList();
		@SuppressWarnings(value = {"unchecked", })
		ArrayList q5 = new ArrayList();
		@SuppressWarnings(value = {"unused", "rawtypes",})
		ArrayList q6 = new ArrayList();
-}
elementValueArrayInitializer :: Parser Token ASTNode
elementValueArrayInitializer = 
    sep OpenCurly            >> 
    opt [] elementValues     >>= \evs -> 
    optional (sep Comma)     >>
    sep CloseCurly           >>
    pure (AEVAInit evs)


elementValue :: Parser Token ASTNode
elementValue = 
    annotation                     <|> 
    expression1                    <|> 
    elementValueArrayInitializer


expression1 = error "TODO"


elementValuePair :: Parser Token ASTNode
elementValuePair = 
    identifier         >>= \i -> 
    op Equals          >>
    elementValue       >>= \ev ->
    pure (APair i ev)


elementValuePairs :: Parser Token [ASTNode]
elementValuePairs = 
    sepBy1Fst elementValuePair (sep Comma)


annotationElement :: Parser Token [ASTNode]
annotationElement = 
    elementValuePairs         <|> 
    fmap (:[]) elementValue


annotation :: Parser Token ASTNode
annotation = 
    pure AAnno             <*>
    (literal AtSign         *> 
     qualifiedIdentifier)  <*> 
     rest
  where
    rest = opt [] (sep OpenParen               *> 
                   opt [] annotationElement   <* 
                   sep CloseParen)


annotations :: Parser Token [ASTNode]
annotations = some annotation


modifier :: Parser Token ASTNode
modifier = annotation <|> others
  where
    others = mconcat $ map q [Kpublic, Kprotected, Kprivate, Kstatic,  
                              Kabstract, Kfinal, Knative, Ksynchronized,  
                              Ktransient, Kvolatile, Kstrictfp]
    q k = key k *> pure (AModifier (tail $ show k))



-- Section 6: class body


constructorDeclaration :: Parser Token ASTNode
constructorDeclaration = 
    pure AConsDecl          <*>
    opt [] typeParameters   <*>
    identifier              <*>
    formalParameters        <*>
    opt [] throwsClause     <*>
    block


variableDeclaration :: Parser Token ASTNode
variableDeclaration =
    pure AVarDecl                   <*>
    identifier                      <*>
    (fmap length $ many array)      <*>
    optional (op Equals  *>
              variableInitializer)


fieldDeclaration :: Parser Token ASTNode
fieldDeclaration = 
    pure AFieldDecl  <*>
    jtype            <*>
    vars             <*
    sep Semicolon
  where
    vars = sepBy1Fst variableDeclaration (sep Comma)


throwsClause :: Parser Token [ASTNode]
throwsClause = 
    key Kthrows               *> 
    qualifiedIdentifierList


methodDeclaration :: Parser Token ASTNode
methodDeclaration =
    opt [] typeParameters       >>= \tps ->
    jtype                       >>= \t ->
    identifier                  >>= \i ->
    formalParameters            >>= \fps ->
    (fmap length $ many array)  >>= \n ->
    opt [] throwsClause         >>= \thrs ->
    (block <|> emptyThing)      >>= \b ->
    pure (AMethodDecl tps (theType t n) i fps thrs b)
  where
    theType (AType a x) n = Just (AType a (x + n))


voidMethodDeclaration :: Parser Token ASTNode
voidMethodDeclaration =
    opt [] typeParameters   >>= \tps ->
    key Kvoid               >>
    identifier              >>= \i ->
    formalParameters        >>= \fps ->
    opt [] throwsClause     >>= \thrs ->
    (block <|> emptyThing)  >>= \b ->
    pure (AMethodDecl tps Nothing i fps thrs b)


memberDecl :: Parser Token ASTNode
memberDecl =
    voidMethodDeclaration  <|>
    methodDeclaration      <|>
    fieldDeclaration       <|>
    classDeclaration       <|>
    interfaceDeclaration   <|>
    constructorDeclaration


classBodyDeclaration :: Parser Token ASTNode
classBodyDeclaration =
    -- TODO:  what about ';' rule ?? it's now in 'classBody'
    member <|> classBlock
  where
    member =
        pure AMemberDecl <*> 
        many modifier    <*> 
        memberDecl
    classBlock = 
        pure AClassBlock                      <*>
        opt False (key Kstatic *> pure True)  <*>
        block


classBody :: Parser Token ASTNode
classBody = 
    sep OpenCurly         >> 
    many decl             >>= \ds -> 
    sep CloseCurly        >>
    pure (AClassBody ds)
  where
    decl = classBodyDeclaration <|> emptyThing



-- Section 7:  interface body

constantDeclaration :: Parser Token ASTNode
constantDeclaration =
    pure AIConstDecl                <*>
    identifier                      <*>
    (fmap length $ many array)      <*>
    (op Equals  *>
     variableInitializer)


intfFieldDecl :: Parser Token ASTNode
intfFieldDecl =
    pure AIFieldDecl  <*>
    jtype             <*>
    constants         <*
    sep Semicolon
  where
    constants = sepBy1Fst constantDeclaration (sep Comma)


intfMethodDecl :: Parser Token ASTNode
intfMethodDecl =
    opt [] typeParameters       >>= \tps ->
    jtype                       >>= \t ->
    identifier                  >>= \i ->
    formalParameters            >>= \fps ->
    (fmap length $ many array)  >>= \n ->
    opt [] throwsClause         >>= \thrs ->
    sep Semicolon               >>
    pure (AIMethodDecl tps (theType t n) i fps thrs)
  where
    theType (AType a x) n = Just (AType a (x + n))


intfVoidMethodDecl :: Parser Token ASTNode
intfVoidMethodDecl =
    opt [] typeParameters  >>= \tps ->
    key Kvoid              >>
    identifier             >>= \i ->
    formalParameters       >>= \fps ->
    opt [] throwsClause    >>= \thrs ->
    sep Semicolon          >>
    pure (AIMethodDecl tps Nothing i fps thrs)


intfMembDecl :: Parser Token ASTNode
intfMembDecl =
    intfVoidMethodDecl    <|>
    intfMethodDecl        <|>
    intfFieldDecl         <|>
    classDeclaration      <|>
    interfaceDeclaration


interfaceBodyDeclaration :: Parser Token ASTNode
interfaceBodyDeclaration =
    pure AIMemberDecl    <*>
    many modifier        <*>
    intfMembDecl


interfaceBody :: Parser Token ASTNode
interfaceBody =
    sep OpenCurly        >>
    many decl            >>= \ds ->
    sep CloseCurly       >>
    pure (AIntfBody ds)
  where
    decl = interfaceBodyDeclaration <|> emptyThing
    


-- Section 8

variableModifier :: Parser Token ASTNode
variableModifier =
    final        <|>
    annotation
  where
    final = key Kfinal *> pure (AModifier "final")


formalParamDecl :: Parser Token ASTNode
formalParamDecl =
    many variableModifier           >>= \mds ->
    jtype                           >>= \(AType t n) ->
    identifier                      >>= \i ->
    (fmap length $ many array)      >>= \n' ->
    pure (AFParam mds (AType t (n + n')) i)


varArg :: Parser Token ASTNode
varArg =
    many variableModifier     >>= \mds ->
    jtype                     >>= \t ->
    literal Ellipsis          >>
    identifier                >>= \i ->
    pure (AFParam mds t i)


formalParameters :: Parser Token ASTNode
formalParameters =
    sep OpenParen          >>
    params                 >>= \(ps,_) ->
    optional varArg        >>= \va ->
    sep CloseParen         >>
    pure (AFParams ps va)
  where
    params = sepBy0 formalParamDecl (sep Comma)


variableInitializer :: Parser Token ASTNode
variableInitializer =
    arrayInitializer    <|>
    expression


arrayInitializer :: Parser Token ASTNode
arrayInitializer =
    sep OpenCurly         >>
    varInits              >>= \(vs,_) ->
    optional (sep Comma)  >>
    pure (AArrayInit vs)
  where
    varInits = sepBy0 variableInitializer (sep Comma)


expression = error "TODO"


-- Section 9: blocks

block :: Parser Token ASTNode
block =
    sep OpenCurly        >>
    many blockStatement  >>= \bs ->
    sep CloseCurly       >>
    pure (ABlock bs)


blockStatement :: Parser Token ASTNode
blockStatement =
    localVarDeclStmnt   <|>
    classDeclaration    <|>
    statement


localVarDeclStmnt :: Parser Token ASTNode
localVarDeclStmnt = 
    pure ALVDStmnt         <*>
    many variableModifier  <*> 
    jtype                  <*>
    vars                   <*
    sep Semicolon
  where
    vars = sepBy1Fst variableDeclaration (sep Comma)


labeledStatement :: Parser Token ASTNode
labeledStatement =
    identifier   >>= \i ->
    op Colon   >>
    statement   >>= \s ->
    pure (ALabel i s)


statementExpression :: Parser Token ASTNode
statementExpression =
    expression     <*
    sep Semicolon


parenExpression :: Parser Token ASTNode
parenExpression = 
    sep OpenParen   >>
    expression      >>= \e ->
    sep CloseParen  >>
    pure e


ifStatement :: Parser Token ASTNode
ifStatement =
    key Kif              >>
    parenExpression      >>= \c ->
    statement            >>= \s ->
    optional elseClause  >>= \e ->
    pure (AIf c s e)
  where
    elseClause = 
        key Kelse  *>
        statement


assertStatement :: Parser Token ASTNode
assertStatement =
    key Kassert           >>
    expression            >>= \e1 ->
    optional other        >>= \e2 ->
    sep Semicolon         >>
    pure (AAssert e1 e2)
  where
    other = 
        op Colon    *>
        expression


-- based on this grammar,
--   which I believe is 100% equivalent:
--  
-- SwitchStatement:
--     switch ( Expression ) SwitchBlock
-- 
-- SwitchBlock:
--     { 
--     many0 SwitchBlockStatementGroup
--     }
-- 
-- SwitchBlockStatementGroup:
--     many1 SwitchLabel 
--     many0 BlockStatement
-- 
-- SwitchLabel:
--     case ConstantExpression :
--     case Identifier :
--     default :
-- 
switchStatement :: Parser Token ASTNode
switchStatement =
    key Kswitch           >>
    parenExpression       >>= \e ->
    sep OpenCurly         >>
    many sbsg             >>= \sgs ->
    sep CloseCurly        >>
    pure (ASwitch e sgs)
  where
    sbsg = 
        pure ASwitchSGs      <*>
        some switchLabel     <*>
        many blockStatement
    switchLabel =
        pure ACase        <*>
        (case' <|> def)   <*
        op Colon
    case' =
        key Kcase                    >>
        (identifier <|> expression)  >>= \e ->
        pure (Just e)
    def =
        key Kdefault >>
        pure Nothing
    


whileLoop :: Parser Token ASTNode
whileLoop =
    key Kwhile         >>
    parenExpression    >>= \e ->
    statement          >>= \s ->
    pure (AWhile e s)


doLoop :: Parser Token ASTNode
doLoop =
    key Kdo             >>
    statement           >>= \s ->
    key Kwhile          >>
    expression          >>= \e ->
    sep Semicolon       >>
    pure (ADoLoop s e)


breakStatement :: Parser Token ASTNode
breakStatement =
    key Kbreak            >>
    optional identifier   >>= \i ->
    sep Semicolon         >>
    pure (ABreak i)


continueStatement :: Parser Token ASTNode
continueStatement =
    key Kcontinue        >>
    optional identifier  >>= \i ->
    sep Semicolon        >>
    pure (AContinue i)


returnStatement :: Parser Token ASTNode
returnStatement =
    key Kreturn          >>
    optional expression  >>= \e ->
    sep Semicolon        >>
    pure (AReturn e)


throwStatement :: Parser Token ASTNode
throwStatement =
    key Kthrow       >>
    expression       >>= \e ->
    sep Semicolon    >>
    pure (AThrow e)


synchStatement :: Parser Token ASTNode
synchStatement = 
    key Ksynchronized   >>
    parenExpression     >>= \e ->
    block               >>= \b ->
    pure (ASynch e b)


tryStatement :: Parser Token ASTNode
tryStatement =
    key Ktry            >>
    block               >>= \b ->
    many catchClause    >>= \cs ->
    optional finally    >>= \f ->
    pure (ATry b cs f)


catchClause :: Parser Token ASTNode
catchClause =
    key Kcatch               >>
    sep OpenParen            >>
    many variableModifier    >>= \ms ->
    identifier               >>= \t ->
    identifier               >>= \i ->
    sep CloseParen           >>
    block                    >>= \b ->
    pure (ACatch ms t i b)


finally :: Parser Token ASTNode
finally = 
    key Kfinally   *>
    block


resource :: Parser Token ASTNode
resource =
    many variableModifier       >>= \ms ->
    referenceType               >>= \t ->
    identifier                  >>= \i ->
    (fmap length $ many array)  >>= \n ->
    op Equals                   >>
    expression                  >>= \e ->
    pure (AResource ms t i n e)


tryResourceStatement :: Parser Token ASTNode
tryResourceStatement =
    key Ktry                             >>
    sep OpenParen                        >>
    sepBy1Fst resource (sep Semicolon)   >>= \rs ->
    sep CloseParen                       >>
    block                                >>= \b ->
    many catchClause                     >>= \cs ->
    optional finally                     >>= \f ->
    pure (ATryResource rs b cs f)
    


statement :: Parser Token ASTNode
statement = 
    block                <|>
    emptyThing           <|>
    labeledStatement     <|>
    statementExpression  <|>
    ifStatement          <|>
    assertStatement      <|>
    switchStatement      <|>
    whileLoop            <|>
    doLoop               <|>
    forLoop              <|>
    forEachLoop          <|>
    breakStatement       <|>
    continueStatement    <|>
    returnStatement      <|>
    throwStatement       <|>
    synchStatement       <|>
    tryStatement         <|>
    tryResourceStatement



forLoop              = error "TODO"
