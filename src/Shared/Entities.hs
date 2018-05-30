{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
-- | Example entities 
module Shared.Entities where
import           GHC.Generics    (Generic)              -- needed to derive type class instances declaratively
import           Data.Aeson      (ToJSON, FromJSON)     -- needed to provide JSON encoding/decoding


data User = User Integer String String String deriving (Show, Read, Generic, ToJSON, FromJSON)

data Post = Post Integer Integer String deriving (Show, Read, Generic, ToJSON, FromJSON)


