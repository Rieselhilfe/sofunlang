module Tape where

data Tape a = Tape [a] a [a]

listToTape (x:xs) = (Tape [] x xs)
listToTape [] = error $ "tried to convert empty list to tape"
tapeToList (Tape x y z) = (reverse x)++(y:z)

moveRight (Tape ls p (r:rs)) = Tape (p:ls) r rs
moveLeft  (Tape (l:ls) p rs) = Tape ls l (p:rs)
lenRight (Tape _ _ xs) = length xs
lenLeft (Tape xs _ _) = length xs
lenAll x = (lenLeft x) + (lenRight x) + 1

instance Functor Tape where
  fmap f (Tape ls p rs) = Tape (map f ls) (f p) (map f rs)

instance Show a => Show (Tape a) where
  show (Tape as x bs) =
    (concatMap show $ reverse as) ++ "[[ " ++ (show x) ++ "]] " ++ (concatMap show bs) 
