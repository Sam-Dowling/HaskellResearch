data RPS = Rock | Paper | Scissors deriving Show
data Tree a = EmptyTree | Tree a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Eq RPS where
    Rock == Rock = True
    Paper == Paper = True
    Scissors == Scissors = True
    _ == _ = False

instance Ord RPS where
    compare Paper Rock = GT 
    compare Scissors Rock = GT 
    compare Scissors Paper = GT
    
    compare Rock Paper = LT
    compare Rock Scissors = LT
    compare Paper Scissors = LT

singleton :: RPS -> Tree RPS
singleton x = Tree x EmptyTree EmptyTree

treeInsert :: RPS -> Tree RPS -> Tree RPS
treeInsert x EmptyTree = singleton x
treeInsert x (Tree a left right)
    | x == a = Tree x left right
    | x < a = Tree a (treeInsert x left) right
    | x > a = Tree a left (treeInsert x right)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Tree x left right) = Tree (f x) (fmap f left) (fmap f right)

