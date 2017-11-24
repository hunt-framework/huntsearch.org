type HuntHandler a = forall master. YesodHunt master
                  => HandlerT HuntS (HandlerT master IO) a

runHunt :: Command -> HuntHandler (Either CmdError CmdResult)
runHunt cmd = do
    env <- getHunt <$> getYesod
    liftIO $ runCmd env cmd

