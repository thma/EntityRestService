{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
module Main where

import           Data.Text
import           Yesod
import           Shared.Entities
import           Server.Backend             (retrieveEntity, storeEntity)
import           Server.HaskellShow
import           Server.ErrorHandler        (hsMimeTypeAwareErrorHandler)
import           Util.Helper                (readFromArgsOrDefault)

main :: IO ()
main = do
    port <- readFromArgsOrDefault 3000
    putStrLn $ "Listening on port " ++ show port
    warp port App

data App = App
instance Yesod App where
    errorHandler = hsMimeTypeAwareErrorHandler

mkYesod "App" [parseRoutes|
/user/#Text    UserR  GET POST
/post/#Text    PostR  GET
|]

-- ROUTE HANDLERS
getUserR :: Text -> Handler TypedContent
getUserR id = do
    user <- liftIO $ (retrieveEntity id :: IO User)
    returnValidRep user

postUserR :: Text -> Handler ()
postUserR id = do
  newUser <- requireJsonBody :: Handler User
  liftIO $ storeEntity id newUser
  return ()

getPostR :: Text -> Handler TypedContent
getPostR id = do
    post <- liftIO $ (retrieveEntity id :: IO Post)
    returnValidRep post

-- | produce a Representation matching the clients accept header settings
returnValidRep  :: (MonadHandler m, Show a, ToJSON a) => a -> m TypedContent
returnValidRep a = selectRep $ do
    -- representation for Mimetype  "application/json"
    provideRep $ returnJson a
    -- representation for Mimetype "text/haskell-show"
    provideRep $ returnShow a
