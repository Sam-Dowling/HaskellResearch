import Control.Monad (liftM)
import Data.Char (isAlpha, toLower)
import Data.List (maximumBy,foldl',intersperse)
import Data.List.Split
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as B

type Model = M.Map B.ByteString Int

mkWords :: B.ByteString -> [B.ByteString]
mkWords = filter (not . B.null) . B.splitWith (not . isAlpha) . B.map toLower


train :: [B.ByteString] -> Model
train = foldl' updateMap M.empty
    where updateMap model word = M.insertWith' (+) word 1 model


edits1 :: B.ByteString -> [B.ByteString]
edits1 word = concat
    [[h `B.append` B.tail t | (h, t) <- hts'],
     [B.concat [h, (B.take 1 . B.drop 1) t, B.take 1 t, B.drop 2 t] | (h, t) <- hts],
     [B.concat [h, B.singleton c, B.tail t] | (h, t) <- hts', c <- alpha],
     [B.concat [h, B.singleton c, t] | (h, t) <- hts, c <- alpha]]
  where hts = [B.splitAt n word | n <- [0..fromIntegral len]]
        hts' = take (len - 1) hts
        len = fromIntegral (B.length word)
        alpha = ['a'..'z']
        

correct :: Model -> B.ByteString -> B.ByteString
correct model word = (maximumBy cmpFreq . head . filter (not . null))
                     (map known [[word], edits1 word, edits2 word] ++ [[word]])
    where known = filter (`M.member` model)
          edits2 = concatMap edits1 . edits1
          findFreq word = M.findWithDefault 1 word model
          cmpFreq a b = compare (findFreq a) (findFreq b)
          
readModel :: FilePath -> IO Model
readModel name = (train . mkWords) `liftM` B.readFile name

toString :: [String] -> String
toString = concat . intersperse " "

spellCheck :: String -> IO (String)
spellCheck word = do
    model <- readModel "words"
    return . toString . map (B.unpack . correct model . B.pack) $ splitOn " " word
