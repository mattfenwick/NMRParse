module JavaParser (

) where

import Java
import MParse
import Classes
import Instances
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)
import JavaTokens



-- ------------------
-- the 'real', fancy parsers
-- they'll be working on [Token]



-- Section 1: identifiers

identifier :: Parser Token Token
identifier = satisfy isIdent
  where
    isIdent (Identifier _)   =  True
    isIdent   _              =  False


qualifiedIdentifier :: Parser Token [Token] 
qualifiedIdentifier = fmap fst $ sepBy1 identifier (literal (Separator Period)) 


qualifiedIdentifierList :: Parser Token [Token]
qualifiedIdentifierList = fmap fst $ sepBy1 identifier (literal (Separator Comma))



-- Section 2:  




-- Section 3:  types

opt :: a -> Parser t a -> Parser t a
opt x p = p <|> pure x


key :: Keyword -> Parser Token Token
key = literal . Keyword


sep :: Separator -> Parser Token Token
sep = literal . Separator


op :: Operator -> Parser Token Token
op = literal . Operator


basicType :: Parser Token Token
basicType = mconcat $ map key [Kbyte, Kshort, Kchar, Kint, Klong, Kfloat, Kdouble, Kboolean]


typeArgument :: Parser Token [Token]
typeArgument = referenceType <|> (fmap (:) (op LessThan) <*> opt [] rest)
  where
    rest = fmap (:) (key Kextends <|> key Ksuper) <*> referenceType


typeArguments :: Parser Token [Token]
typeArguments = op LessThan *> fmap (concat . fst) (sepBy1 typeArgument (sep Comma)) <* op GreaterThan


referenceType :: Parser Token [Token]
referenceType = fmap (concat . fst) $ sepBy1 main (sep Period)
  where
    main = fmap (:) identifier <*> opt [] typeArguments


jtype :: Parser Token [Token]
jtype = fmap (++) ( fmap (:[]) basicType <|> referenceType) <*> opt [] array
  where
    array = fmap (\x y -> [x,y]) (sep OpenSquare) <*> sep CloseSquare


-- Section 4: generics

bound :: Parser Token [Token]
bound = fmap (concat . fst) $ sepBy1 referenceType (op And)


typeParameter :: Parser Token [Token]
typeParameter = fmap (\x y z -> x:y:z) identifier <*> key Kextends <*> bound


typeParameters :: Parser Token [Token]
typeParameters = op LessThan *> main <* op GreaterThan
  where
    main = fmap (concat . fst) $ sepBy1 typeParameter (sep Comma)


nonWildcardTypeArguments :: Parser Token [Token]
nonWildcardTypeArguments = op LessThan *> typeList <* op GreaterThan


typeList :: Parser Token [Token]
typeList = fmap (concat . fst) $ sepBy1 referenceType (sep Comma)


diamond :: Parser Token [Token]
diamond = pure [] <* op LessThan <* op GreaterThan


typeArgumentsOrDiamond :: Parser Token [Token]
typeArgumentsOrDiamond = diamond <|> typeArguments


nonWildcardTypeArgumentsOrDiamond :: Parser Token [Token]
nonWildcardTypeArgumentsOrDiamond = diamond <|> nonWildcardTypeArguments



-- Section 5:

elementValues :: Parser Token [Token]
elementValues = fmap (concat . fst) $ sepBy1 elementValue (sep Comma)


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
elementValueArrayInitializer :: Parser Token [Token]
elementValueArrayInitializer = sep OpenSquare *> main <* optional (sep Comma) <* sep CloseSquare
  where
    main = fmap (concat . fst) $ sepBy0 elementValue (sep Comma)


elementValue :: Parser Token [Token]
elementValue = annotation <|> elementValueArrayInitializer
-- oops, don't have 'expression1' yet
 -- annotation <|> expression1 <|> elementValueArrayInitializer
 
 
elementValuePair :: Parser Token [Token]
elementValuePair = fmap (\x _ y -> x:y) identifier <*> op Equals <*> elementValue


elementValuePairs :: Parser Token [Token]
elementValuePairs = fmap (concat . fst) $ sepBy1 elementValuePair (sep Comma)


annotationElement :: Parser Token [Token]
annotationElement = elementValuePairs <|> elementValue


annotation :: Parser Token [Token]
annotation = fmap (\_ xs ys -> xs ++ ys) (literal AtSign) <*> qualifiedIdentifier <*> rest
  where
    rest = opt [] (sep OpenParen *> opt [] annotationElement <* sep CloseParen)


annotations :: Parser Token [Token]
annotations = fmap concat $ some annotation


modifier :: Parser Token [Token]
modifier = annotation <|> fmap (:[]) others
  where
    others = mconcat $ map key [Kpublic, Kprotected, Kprivate, Kstatic, Kabstract, Kfinal, Knative, Ksynchronized, Ktransient, Kvolatile, Kstrictfp]



-- 








{-
classOrInterfaceType :: ???
classOrInterfaceType = pseq3 f identifier (optional TypeArguments <what value??>) classOrInterfaceTypeRest
  where f = ???


arrayType :: Parser Token Token
arrayType = ignoreRight ptype $ pseq (literal $ Separator OpenSquare) (literal $ Separator CloseSquare)


referenceType :: Parser Token Token
referenceType = alt classOrInterfaceType arrayType


primitiveType :: Parser Token Token
primitiveType = pany $ map (literal . Keyword) [Kbyte, Kshort, Kint, Klong, Kchar, Kfloat, Kdouble, Kboolean]


ptype :: Parser Token Token
ptype = alt primitiveType referenceType




{-


-- constantExpression ?

leftHandSide :: Parser Token [Token]
leftHandSide = pany [expressionName, fieldAccess, arrayAccess]

-}
-}