{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module Server.HaskellShow where

import           Yesod
import           Data.Text

hsMimeType :: ContentType
hsMimeType = "text/haskell-show"

data HaskellShow = forall a. Show a => HaskellShow a
instance ToContent HaskellShow where
    toContent (HaskellShow x) = toContent $ show x
instance ToTypedContent HaskellShow where
    toTypedContent = TypedContent hsMimeType . toContent
instance HasContentType HaskellShow where
    getContentType _ = hsMimeType

returnShow :: (Show a, Monad m) => a -> m HaskellShow
returnShow = return . HaskellShow