module Data.Reversed
       (Reversed (..)
       , consR
       ) where

import Data.Function

newtype Reversed a = Reversed { getReversed :: a }
                     deriving (Eq, Show)

instance Ord a => Ord (Reversed a) where
  compare x y = oppOrd ((compare `on` getReversed) x y)
    where oppOrd LT = GT
          oppOrd GT = LT
          oppOrd EQ = EQ

consR :: Reversed [a] -> a -> Reversed [a]
xs `consR` x = Reversed $ x : getReversed xs
