import Data.List

ccount :: Char -> String -> Int
ccount c = length . filter (== c)

cprofile :: Char -> [String] -> [Int]
cprofile c = map (ccount c)

type ProfileEntry = (Char, [Int])
type Profile = [ProfileEntry]

profileEntry c ll = (c, cprofile c ll)

profile :: [String] -> Profile
profile ll = map ($ ll) $ map profileEntry ['A', 'C', 'G', 'T']

profileSplit :: ProfileEntry -> [(Char, Int)]
profileSplit (c, l) = map (\x -> (c, x)) l

profileTranspose :: Profile -> [[(Char, Int)]]
profileTranspose = transpose . map profileSplit

maxChar :: [(Char, Int)] -> Char
maxChar l = maxCharIter l 0 'A'
  where
    maxCharIter [] _ c = c
    maxCharIter ((c, d):xs) md mc
      | d > md = maxCharIter xs d c
      | otherwise = maxCharIter xs md mc
                    
consensus :: Profile -> String
consensus = map maxChar . profileTranspose

profileEntryToString :: ProfileEntry -> String
profileEntryToString (c, l) = (show c) ++ ": " ++ (intercalate " " $ map show l)

profileToString :: Profile -> String
profileToString = intercalate "\n" . map profileEntryToString

readInput :: String -> [String]
readInput = transpose . lines

answer :: String -> String
answer s = 
  let p = profile $ readInput s
      c = consensus p
  in
   c ++ "\n" ++ (profileToString p)
   
doWork :: String -> String -> IO ()
doWork fileName outFileName = do
  input <- readFile fileName
  let s = answer input
    in
   writeFile outFileName s