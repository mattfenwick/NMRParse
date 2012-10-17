module StarData (

) where

import qualified Data.Map as Map


data DLoop = DLoop [String] [[String]] deriving (Show, Eq)


data DSaveBlock = DSaveBlock (Map.Map String String) [DLoop] deriving (Show, Eq)


data DDataBlock = DDataBlock String (Map.Map String DSaveBlock) deriving (Show, Eq)


data DStar = DStar DDataBlock deriving (Show, Eq)



makeLoop :: [String] -> [[String]] -> Either String Loop
makeLoop keys valLists
  | all ((==) (length keys)) (map length valLists) = Right $ Loop keys valLists
  | otherwise = Left "all value lists' lengths must exactly match the number of keys"
  

starToLoop :: [String] -> [String] -> Either String Loop
starToLoop keys values = makeLoop keys (chunkList (length keys) values)


chunkList :: Int -> [a] -> [[a]]
chunkList size elems
  | size <= 0 = error "list size must be positive"
  | length elems <= size = [elems]
  | otherwise = (take size elems):(chunkList size $ drop size elems)