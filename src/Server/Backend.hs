{-# LANGUAGE ScopedTypeVariables #-}
module Server.Backend
    (
      retrieveEntity
    ) where

import           Control.Exception          hiding (Handler)
import           Yesod
import           Data.Aeson
import           Data.Text
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class  (lift)
import qualified Data.ByteString.Lazy       as B
import           System.Directory           (doesFileExist)
import           Control.DeepSeq            (rnf)
import           Text.Read                  (readEither)
import           Data.Typeable

-- | load a persistent entity of type t and identified by id from the backend
retrieveEntity :: forall a. (FromJSON a, Read a, Typeable a) => Text -> IO a
retrieveEntity id = do
    -- in our simple server documents can be either JSON or Read (i.e. plain text)
    let jsonFileName = getPath (typeRep ([] :: [a])) id ".json"
    let textFileName = getPath (typeRep ([] :: [a])) id ".txt"
    useJson <- doesFileExist jsonFileName
    result <- if useJson
                then
                    parseFromJsonFile jsonFileName :: FromJSON a => IO a
                else
                    parseFromTxtFile  textFileName :: Read a => IO a
    return result

-- | compute path of data file
getPath :: TypeRep -> Text -> String -> String
getPath tr id ex = "data/" ++ show tr ++ "." ++ unpack id ++ ex

-- | read String from file fileName and then parse the contents as a Read instance.
parseFromTxtFile :: Read a => FilePath -> IO a
parseFromTxtFile fileName = do
    contentString <- readFile fileName
    return $ read contentString

-- | read from file fileName and then parse the contents as a FromJSON instance.
parseFromJsonFile :: FromJSON a => FilePath -> IO a
parseFromJsonFile fileName = do
    contentBytes <- B.readFile fileName
    case eitherDecode contentBytes of
        Left msg -> fail msg
        Right x  -> return x
