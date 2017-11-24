data HuntS = HuntS { getHunt :: DefHuntEnv }

initHuntS :: IO HuntS
initHuntS = HuntS <$> initHunt

class Yesod master => YesodHunt master where

mkYesodSubData "HuntS" [parseRoutes|
/search/#String HSearch GET
/search/#String/#Int/#Int HPagedSearch GET
/completion/#String HCompletion GET
|]

