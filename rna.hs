rna str = map d str
  where
    d 'T' = 'U'
    d c = c