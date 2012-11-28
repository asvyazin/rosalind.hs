import Fasta

percent :: Int -> Int -> Float
percent n full =
  (nF / fullF) * 100
  where
    nF = fromIntegral n
    fullF = fromIntegral full
  
gc :: String -> Float
gc str =
  percent ngc len
  where
    len = length str
    ngc = length $ filter isGC str
    isGC 'G' = True
    isGC 'C' = True
    isGC _ = False

type GCEntry = (String, Float)
type GCDataset = [GCEntry]

gcEntry :: Entry -> GCEntry
gcEntry (k, s) = (k, gc s)

gcDataset :: Dataset -> GCDataset
gcDataset d = map gcEntry d

maxGC :: GCDataset -> GCEntry
maxGC [x] = x
maxGC (x:xs) = maxGC2 x $ maxGC xs
  where
    maxGC2 e1@(k1, v1) e2@(k2, v2)
      | v1 > v2 = e1
      | otherwise = e2
                    
answer :: String -> GCEntry
answer = maxGC . gcDataset . parseFasta

answerIO :: String -> IO ()
answerIO filename = do
  str <- readFile filename
  print $ answer str