instance Index IntIndex where
  type IKey IntIndex v = Text
  type IVal IntIndex v = Occurrences

  insertList wos (IntIx i)
    = mkIntIx $ insertList wos i
-- ...
