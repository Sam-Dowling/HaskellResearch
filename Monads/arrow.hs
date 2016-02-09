{-# LANGUAGE Arrows #-}

import Control.Arrow
import Control.Category
import Prelude hiding (id, (.))

newtype SimpleFunc a b = SimpleFunc {
    runF :: (a -> b)
}


instance Arrow SimpleFunc where
    arr f = SimpleFunc f
    first (SimpleFunc f) = SimpleFunc (mapFst f)
                  where mapFst g (a,b) = (g a, b)
    second (SimpleFunc f) = SimpleFunc (mapSnd f)
                  where mapSnd g (a,b) = (a, g b)


instance Category SimpleFunc where
    (SimpleFunc g) . (SimpleFunc f) = SimpleFunc (g . f)
    id = arr id

split :: (Arrow a) => a b (b, b)
split = arr (\x -> (x,x))

unsplit :: (Arrow a) => (b -> c -> d) -> a (b, c) d
unsplit = arr . uncurry       

liftA2 :: (Arrow a) => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = split >>> first f >>> second g >>> unsplit op

f, g :: SimpleFunc Int Int
f = arr (`div` 2)
g = arr (\x -> x*3 + 1)

h :: SimpleFunc Int Int
h = liftA2 (+) f g

hOutPut :: Int
hOutPut = runF h 8
