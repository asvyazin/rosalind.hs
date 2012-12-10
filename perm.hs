import Data.List

intsToString :: [Int] -> String
intsToString = unwords . map show

answerToString :: [[Int]] -> String
answerToString = intercalate "\n" . map intsToString

toString :: Int -> String
toString n = show (length perm) ++ "\n" ++ answerToString perm
  where
    perm = permutations [1..n]

answer :: Int -> String -> IO ()
answer n fileName =
  writeFile fileName $ toString n
  