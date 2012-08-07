
import qualified Data.List as List
import Control.Monad.Error


char :: Char -> String -> Either String (String, String)
char _ [] = Left "empty string"
char c (x:xs)
  | c == x = Right ([x], xs)
  | otherwise = Left "char did not match"


pnot :: Char -> String -> Either String (String, String)
pnot _ [] = Left "empty string"
pnot c (x:xs)
  | c /= x = Right ([x], xs)
  | otherwise = Left "char *did* match"


zero_or_more :: (String -> Either String (String, String)) -> String -> Either String (String, String)
zero_or_more _ "" = Right("", "")
zero_or_more p xs = next (p xs)
  where
    next (Left mess) = Right ("", xs)
    next (Right (matched, rest)) = screwWith $ zero_or_more p rest
      where 
        screwWith (Left m) = Left m
        screwWith (Right (m, r)) = Right (matched ++ m, r)


one_or_more :: (String -> Either String (String, String)) -> String -> Either String (String, String)
one_or_more p xs = do
  (match1, rest1) <- p xs
  (match2, rest2) <- zero_or_more p rest1
  return (match1 ++ match2, rest2)


string :: String -> String -> Either String (String, String)
string search input = whatever $ List.stripPrefix search input
  where
    whatever (Just x) = Right (search, x)
    whatever Nothing = Left $ "couldn't match string " ++ search ++ ", " ++ input


pand :: (String -> Either String (String, String)) -> (String -> Either String (String, String)) -> String -> Either String (String, String)
pand l r input = do
  (match1, rest1) <- l input
  (match2, rest2) <- r rest1
  return (match1 ++ match2, rest2)


por :: (String -> Either String (String, String)) -> (String -> Either String (String, String)) -> String -> Either String (String, String)
por l r input = hmmmm $ l input
  where
    hmmmm (Left _) = r input
    hmmmm x = x


-- example:  `lookahead (char 'c') ((== 'd') . head) "cdef"`
-- 
lookahead :: (String -> Either String (String, String)) -> (String -> Bool) -> String -> Either String (String, String)
lookahead p l xs = yes $ p xs
  where
    yes (Left m) = Left m
    yes (Right (match, rest)) 
      | l rest = Right (match, rest)
      | otherwise = Left $ "matched " ++ match ++ " but failed lookahead on: " ++ rest


{-|
one_or_more :: (String -> Either (String, String) String) -> String -> Either (String, String) String
one_or_more _ [] = Right "empty string"
one_or_more p s = ??? repeat match (p s) ???
  where
    match (Left (matched, next)) = 
    match (Right message) = 
-}

-- str :: String -> Either (String, String) String
-- str input = 
