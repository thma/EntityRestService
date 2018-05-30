{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
module Server.ErrorHandler
    (
      hsMimeTypeAwareErrorHandler
    ) where
import           Yesod
import           Data.Text
import           Server.HaskellShow


hsMimeTypeAwareErrorHandler :: Yesod site => ErrorResponse -> HandlerT site IO TypedContent
hsMimeTypeAwareErrorHandler (InternalError e) = do
    $logErrorS "yesod-core" e
    selectRep $ do
        -- html
        provideRep $ defaultLayout $ defaultMessageWidget
            "Internal Server Error"
            [hamlet|<pre>#{e}|]
        -- JSON
        provideRep $ return $ object ["message" .= ("Internal Server Error" :: Text), "error" .= e]
        -- HaskellShow
        provideRepType hsMimeType $ return $ HaskellShow $ "Internal Server Error: " ++ show e
-- fall back to default handling
hsMimeTypeAwareErrorHandler other = defaultErrorHandler other

