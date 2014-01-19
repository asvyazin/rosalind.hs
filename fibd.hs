import Control.Applicative
import Control.Monad.Trans.State
import qualified Data.Map as M

type Cache = M.Map Integer Integer

solutionCached :: Integer -> Integer -> State Cache Integer
solutionCached 0 _ = return 1
solutionCached 1 _ = return 1
solutionCached n m | n < 0 = return 0
                   | otherwise = do
                     cache <- get
                     case M.lookup n cache of
                       Just p -> return p
                       Nothing -> do
                         s1 <- solutionCached (n - 1) m
                         s2 <- solutionCached (n - 2) m
                         s3 <- solutionCached (n - m) m
                         let res = s1 + s2 - s3
                         modify $ M.insert n res
                         return res

solution :: Integer -> Integer -> Integer
solution n m = evalState (solutionCached n m) M.empty

main :: IO ()
main = do
  [n, m] <- map (read :: String -> Integer) <$> words <$> getLine
  print $ solution n m
