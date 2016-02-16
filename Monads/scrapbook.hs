



data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


freeTree :: Tree Char
freeTree = 
   Node 'P'  
        (Node 'O'  
            (Node 'L'  
                (Node 'N' Empty Empty)  
                (Node 'T' Empty Empty)  
            )  
            (Node 'Y'  
                (Node 'S' Empty Empty)  
                (Node 'A' Empty Empty)  
            )  
        )  
        (Node 'L'  
            (Node 'W'  
                (Node 'C' Empty Empty)  
                (Node 'R' Empty Empty)  
            )  
            (Node 'A'  
                (Node 'A' Empty Empty)  
                (Node 'C' Empty Empty)  
            )  
        )

data Direction = L | R deriving (Show)
type Directions = [Direction]


changeToP :: Directions-> Tree Char -> Tree Char  
changeToP (L:ds) (Node x l r) = Node x (changeToP ds l) r  
changeToP (R:ds) (Node x l r) = Node x l (changeToP ds r)  
changeToP [] (Node _ l r) = Node 'X' l r  


elemAt :: Directions -> Tree a -> a  
elemAt (L:ds) (Node _ l _) = elemAt ds l  
elemAt (R:ds) (Node _ _ r) = elemAt ds r  
elemAt [] (Node x _ _) = x




{-

data Trivial a = Cow

instance Monad Trivial where
    _ >>= _ = Cow
    return _ = Cow

-}

lamda :: Int -> [Int]
lamda n = map (\x -> x * 2) [1..n]

type Algebra f a = f a -> a 
newtype Mu f = InF { outF :: f (Mu f) } 
cata :: Functor f => Algebra f a -> Mu f -> a 
cata f = f . fmap (cata f) . outF



returnTwo :: Int -> (Int,Int)
returnTwo n = (n,n+1)


{-
type myIO = IO 
type myInt = Int
data ioOrInt = myIO | myInt

ioTest :: Int -> Int
ioTest x = 
-}


check :: String -> Bool
check s
    | length s < 5 = True
    | otherwise = False


shout :: String -> String
shout str = (++"!") str

everySecond :: (a -> a) -> [a] -> [a]
everySecond f (a:b:c) = a:(f b):(everySecond f c)
everySecond f c = c

digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs(x `div` 10) ++ [x `mod` 10] 


void :: a -> ()
void _ = ()


takeEither :: Either Int String -> Int
takeEither = either (+2) length


square x = x * x
printSquare1 = fmap (fmap square) $ Just [1..5]
printSquare2 = (fmap . fmap) square $ Just [1..5]



validate_isbn :: [Int] -> Bool
validate_isbn x = (== 0) . (`mod` 11) . sum . zipWith (*) [1..] $ reverse x



safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x
