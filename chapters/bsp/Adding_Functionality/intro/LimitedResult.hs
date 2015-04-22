data LimitedResult x = LimitedResult
    { lrResult :: [x]
    , lrOffset :: Int
    , lrMax    :: Int
    , lrCount  :: Int
    }
