module Fasta (Dataset, Entry, parseFasta) where

import Text.ParserCombinators.Parsec

type Entry = (String, String)
type Dataset = [Entry]

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

parseFasta :: String -> Dataset
parseFasta s =
  case parse dataset "GC dataset" s of
    Left err -> error $ show err
    Right x -> x
