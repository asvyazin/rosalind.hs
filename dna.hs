dna str = [d str 'A', d str 'C', d str 'G', d str 'T']
  where
    d str c = length $ filter (== c) str