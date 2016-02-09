import Data.Monoid


newtype Add a = Add {getAdd :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Add a) where
    mempty = Add 0
    Add x `mappend` Add y = Add (x + y)


main = print . mconcat $ map Add [1..5]
