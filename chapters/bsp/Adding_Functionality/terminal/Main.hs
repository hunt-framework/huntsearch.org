main :: IO ()
main = do
  args <- getArgs
  ix <- initHunt :: IO DefHuntEnv
  res <- runCmd ix $ LoadIx (head args)
  case res of
    Left err -> error $ show err
     _ -> getQuery ix

getQuery :: DefHuntEnv -> IO ()
getQuery ix = do
  putStrLn "Enter query or ’exit’:"
  qry <- getLine
  unless (qry == "exit") $ do
    case parseQuery qry of
      Right query -> do
        res <- runCmd ix $ Search query 0 100
        print res
        getQuery ix
      Left err -> do
  putStrLn ("Invalid input: " ++ unpack err)
