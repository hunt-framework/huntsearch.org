    get "/" $ fileWithMime "index.html" "text/html"
    get "/search" $ do
        query <- param "query"
        results <- liftIO
            $ withHuntServer "http://localhost:3000"
            $ getQuery query 20 0
        html $ "<table>"
            <> (TL.unlines $ render `fmap` (lrResult results))
            <> "</table>"
    get "/opensearch.xml" $
        fileWithMime "opensearch.xml"
            "application/opensearchdescription+xml"
    get "/completion" $ do
        query <- param "query"
        results <- liftIO
            $ withHuntServer "http://localhost:3000"
            $ getAutocomplete query
        let (Right q) = parseQuery (T.unpack query)
            completions = fmap printQuery
                $ completeQueries q results
        json (query, completions)
