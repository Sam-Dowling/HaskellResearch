import Data.Char

h:: [a] -> Maybe a
h [] = Nothing
h (x:_) = Just x

k :: Char -> Int
k = ord

-- h . fmap k == fmap k . h
-- 
-- h $ fmap k ['a']
-- fmap k $ h ['a']
--         = Just 97
