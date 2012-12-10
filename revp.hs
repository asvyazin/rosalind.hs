rev 'C' = 'G'
rev 'G' = 'C'
rev 'A' = 'T'
rev 'T' = 'A'

isReversePalindrome s = s1 == s
  where
    s1 = reverse $ map rev s
    
type Entry = (Int, Int)

testEntry :: Entry -> String -> Bool
testEntry (n, m) = isReversePalindrome . take m . drop (n - 1)

entries :: Int -> [Entry]
entries k = concatMap (\i -> map (\j -> (i, j)) [4..(right i k)]) [1..(k - 3)]
  where
    right i k = min 12 (k - i + 1)

computeAnswer :: String -> [Entry]
computeAnswer s = filter (`testEntry` s) $ entries (length s)

answer :: String -> IO ()
answer input =
  mapM_ printEntry $ computeAnswer input
    where
      printEntry (n, k) = putStrLn $ show n ++ " " ++ show k