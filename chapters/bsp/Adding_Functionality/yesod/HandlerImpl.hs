getHPagedSearch :: String -> Int -> Int -> HuntHandler Value
getHPagedSearch query start count
  = case parseQuery query of
  Left parseErr -> return . toJSON $ parseErr
  Right qry -> do
    res <- runHunt (Search qry start count)
    case res of
       Right (ResSearch docs) -> return . toJSON $ docs
       Left (ResError code msg) -> return . toJSON $ msg

