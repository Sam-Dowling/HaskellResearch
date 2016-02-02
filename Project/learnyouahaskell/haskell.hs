circumference' :: Double -> Double
circumference' r = 2 * pi * r


lucky :: (Integral a) => a -> a
lucky 7 = 7
lucky x = 0

-- fac 3 = 3 + 3 + 3 = 6
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


charName :: Char -> String
charName 'a' = "Adam"
charName x = "NOPE"



first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, y, _) = y

third :: (a,b,c) -> c
third (_, _, z) = z

add_pos (a,b,c) = first((a,b,c))


head' :: [a] -> a
head' [] = error "NOPE"
head' (x:_) = x

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs



capital :: String -> String
capital "" = "NOPE"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]



test :: String -> String
test "" = "NOPE"
test all@(x:xs) = "all " ++ all ++ " x " ++ [x] ++ " xs " ++ xs


bmi :: (RealFloat a) => a -> String
bmi b
    | b <= 1 = "1"
    | b <= 2 = "2"
    | b <= 3 = "3"
    | otherwise = "NOPE"
    

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b




cyl :: (RealFloat a) => a -> a -> a
cyl r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in sideArea + 2 * topArea


maximum' :: (Ord a) => [a] -> a
maximum' [] = error "NOPE"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

take' :: (Num i, Ord i) => i -> [i] -> [i]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs


rev :: (Num i) => [i] -> [i]
rev [] = []
rev (x:xs) = rev xs ++ [x]


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
        let smallerSort = quicksort [a | a <- xs, a <= x]
            biggerSort = quicksort [a | a <- xs, a > x]
        in smallerSort ++ [x] ++ biggerSort


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)



flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x



largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n 
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n * 3 + 1)


numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15


numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

--sum' :: (Num a) => [a] -> a
--sum' xs = foldl (\acc x -> acc + x) 0 xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs  




