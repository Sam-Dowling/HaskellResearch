import Data.Conduit
import qualified Data.Conduit.List as CL


main = do
    result <- CL.sourceList [1..10] $$ CL.fold (+) 0
    print result
    
    result <- CL.sourceList [1..10] $$ CL.map (+ 1) =$ CL.consume
    print result
