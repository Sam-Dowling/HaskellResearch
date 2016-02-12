import Control.Parallel
import Control.Parallel.Strategies

multiply2seq = parMap rseq (*5) [1.1,1.2..10000]
multiply2 = map (*5) [1.1,1.2..10000]

main = print multiply2
