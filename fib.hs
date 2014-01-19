import Control.Applicative

solution :: Integer -> Integer -> Integer
solution 1 _ = 1
solution 2 _ = 1
solution n k = fst $ myIterate (n - 2) (next k) (1, 1)

myIterate :: Integer -> (a -> a) -> a -> a
myIterate 0 _ s = s
myIterate n f s = myIterate (n - 1) f (f s)

next :: Integer -> (Integer, Integer) -> (Integer, Integer)
next k (n1, n2) = (n1 + k * n2, n1)

main :: IO ()
main = do
  [n, k] <- map (read :: String -> Integer) <$> words <$> getLine
  print $ solution n k
