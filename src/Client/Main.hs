{-# LANGUAGE OverloadedStrings #-}
-- | Simple test client for the Entity REST service
module Main where
import           Data.Aeson
import qualified Data.ByteString.Lazy as Lazy
import           Network.HTTP.Conduit (simpleHttp)
import           Util.Helper          (readFromArgsOrDefault)
import           Control.Monad        (liftM)
import           Shared.Entities

-- | IO action to retrieve json data as bytestring via http 
getJSON :: String -> IO Lazy.ByteString
getJSON port = simpleHttp $ "http://127.0.0.1:" ++ port ++ "/user/4711"

-- | send a request to the billing REST service, and unmarshall the response.   
-- | In case of an Error report the error.
-- | Otherwise print out the unmarshalled Store data 
main :: IO ()
main = do
  port <- show <$> readFromArgsOrDefault 3000
    -- call REST service and unmarshall JSON data into Store instance
  json <- getJSON port
  case eitherDecode json :: (Either String User) of
    Left err -> putStrLn err -- report error
    Right user -> print user     -- print user to StdOut
