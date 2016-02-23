import Data.Monoid

newtype Adder a = Adder a deriving Show

instance Num a => Monoid (Adder a) where
    mempty = Adder 0
    Adder x `mappend` Adder y = Adder (x + y)

endoMap :: Endo Int
endoMap = mconcat $ map Endo [(+1), (*2), negate]

run = mconcat . map Adder $ [endoMap `appEndo` x | x <- [1..10]]





