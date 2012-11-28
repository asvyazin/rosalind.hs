import Data.List
import Fasta

isConnected k s1 s2 = (s1 /= s2) && (suffix == prefix)
  where
    takeTail n l = drop ((length l) - n) l
    suffix = takeTail k s1
    prefix = take k s2
    
type Edge = (String, String)
type Graph = [Edge]

cartesianProduct :: [a] -> [(a, a)]
cartesianProduct [] = []
cartesianProduct (x:xs) = (x, x):((product x xs) ++ (cartesianProduct xs))
  where
    product e l = (map (\x -> (e, x)) l) ++ (map (\x -> (x, e)) l)

computeAnswer :: Dataset -> Graph
computeAnswer = map toEdge . filter isEntryConnected . cartesianProduct
  where
    isEntryConnected (e1@(_, d1), e2@(_, d2)) = isConnected 3 d1 d2
    toEdge (e1, e2) = (fst e1, fst e2)
    
answer :: String -> IO ()
answer fileName = do
  input <- readFile fileName
  mapM_ printEdge (computeAnswer $ parseFasta input)
  where
    printEdge (s1, s2) = do
      putStrLn (s1 ++ " " ++ s2)
