import Data.List

positions :: String -> String -> [Int]
positions s t = reverse $ positionsIter s t 1 []
  where
    positionsIter [] _ _ r = r
    positionsIter s@(x:xs) t n r
      | isPrefixOf t s = positionsIter xs t (n + 1) (n:r)
      | otherwise = positionsIter xs t (n + 1) r