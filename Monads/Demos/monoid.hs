import Data.Monoid


newtype Adder a = Adder {getAdder :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Adder a) where
    mempty = Adder 0
    Adder x `mappend` Adder y = Adder (x + y)


endoFunc :: Endo Int
endoFunc = mconcat $ map Endo [(+1), (*(-2)), negate]


main = print . getAdder . mconcat $ map Adder [endoFunc `appEndo` x | x <- [1..5]]



