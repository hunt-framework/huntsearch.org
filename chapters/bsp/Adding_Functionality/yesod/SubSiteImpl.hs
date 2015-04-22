import Control.Applicative
import Yesod
import Yesod.Hunt

data App = App
  { index :: HuntS
  }

mkYesod "App" [parseRoutes|
/ HomeR GET
/hunt/ HuntSR HuntS index
|]

instance Yesod App
instance YesodHunt App

main :: IO ()
main = do
  app <- App <$> initHuntS
  warp 3000 app

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Welcome to Yesod with Hunt|]

