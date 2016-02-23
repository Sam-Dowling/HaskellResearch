
data MyData = DataA | DataB | DataC | None deriving Show

type DataM = ([MyData], Bool)

(>=>) :: ([MyData] -> DataM) -> ([MyData] -> DataM) -> ([MyData] -> DataM)
m1 >=> m2 = \x ->
    let (y, s1) = m1 x
        (z, s2) = m2 y
    in (z, s1 && s2)
    
result :: MyData -> [MyData] -> DataM
result None x = (None : x, False)
result d x = (d : x, True)

functionA :: [MyData] -> DataM
functionA x = result DataA x -- Pass!

functionB :: [MyData] -> DataM
functionB x = result None x -- This function fails!

functionC :: [MyData] -> DataM
functionC x = result DataC x -- Pass!

pipeline = functionA >=> functionB >=> functionC

run = pipeline []
-- expecting ([None,  None, DataA], False)
-- getting   ([DataC, None, DataA], False)
