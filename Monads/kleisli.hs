
data MyData = DataA | DataB | DataC deriving Show

type DataM = ([MyData], Bool)

(>=>) :: ([MyData] -> DataM) -> ([MyData] -> DataM) -> ([MyData] -> DataM)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 && s2)
    
passM :: MyData -> [MyData] -> DataM
passM d x = (d : x, True)

failM :: [MyData] -> DataM
failM x = (x, False)


functionA :: [MyData] -> DataM
functionA = passM DataA -- Pass!

functionB :: [MyData] -> DataM
functionB = failM -- This function fails!

functionC :: [MyData] -> DataM
functionC = passM DataC -- Pass!


pipeline = functionA >=> functionB >=> functionC

run = pipeline []
