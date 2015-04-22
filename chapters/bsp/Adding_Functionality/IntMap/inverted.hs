cRealInt :: ContextType
cRealInt = CType
  { ctName     = "realInt"
  , ctRegEx    = "([-]?[0-9]*)"
  , ctValidate = CValidator $ isInt
  , ctIxImpl   = mkIndex (Ix.empty :: IntIndex Occurrences)
  }

instance Bijection Int Text where
  from = getInt
  to = T.pack . show

newtype IntIndex _v
  = IntIx { intIx :: KeyProxyIndex Text IntMap Occurrences }
  deriving (Eq, Show, NFData, Typeable)

mkIntIx :: KeyProxyIndex Text IntMap Occurrences
        -> IntIndex _v
mkIntIx x = IntIx $! x
