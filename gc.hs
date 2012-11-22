import Text.ParserCombinators.Parsec

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
    
type Entry = (String, String)
type Dataset = [Entry]
type GCEntry = (String, Float)
type GCDataset = [GCEntry]

dataset :: CharParser () Dataset
dataset = many entry

entry :: CharParser () Entry
entry = do
  char '>'
  k <- key
  char '\n'
  v <- value
  return (k, v)
  
key :: CharParser () String
key = many1 (letter <|> digit <|> char '_')

value :: CharParser () String
value = many1 (letter <|> char '\n') >>= return . glue

glue :: String -> String
glue str = filter (/= '\n') str

gcEntry :: Entry -> GCEntry
gcEntry (k, s) = (k, gc s)

parseGC :: String -> Dataset
parseGC s =
  case parse dataset "GC dataset" s of
    Left err -> error $ show err
    Right x -> x
    
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
answer = maxGC . gcDataset . parseGC

answerIO :: String -> IO ()
answerIO filename = do
  str <- readFile filename
  print $ answer str