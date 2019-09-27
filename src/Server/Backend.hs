{-# LANGUAGE ScopedTypeVariables #-}
module Server.Backend
    (
      retrieveEntity
    , storeEntity
    ) where

import           Control.Exception          hiding (Handler)
import           Yesod
import           Data.Aeson
import           Data.Aeson.Text (encodeToTextBuilder)
import           Data.Text
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (toLazyText)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Class  (lift)
import qualified Data.ByteString.Lazy       as B
import           System.Directory           (doesFileExist)
import           Control.DeepSeq            (rnf)
import           Text.Read                  (readEither)
import           Data.Typeable


-- | load persistent entity of type a and identified by id from the backend
retrieveEntity :: forall a. (FromJSON a, Read a, Typeable a) => Text -> IO a
retrieveEntity id = do
  let jsonFileName = getPath (typeRep ([] :: [a])) id ".json"
  let textFileName = getPath (typeRep ([] :: [a])) id ".txt"
  useJson <- doesFileExist jsonFileName
  if useJson
    then parseFromJsonFile jsonFileName :: FromJSON a => IO a
    else parseFromTxtFile textFileName :: Read a => IO a

-- | store persistent entity of type a and identified by id to the backend
storeEntity :: forall a. (ToJSON a, Show a, Typeable a) => Text -> a -> IO ()
storeEntity id entity = do
  let textFileName = getPath (typeRep ([] :: [a])) id ".txt"
  let jsonFileName = getPath (typeRep ([] :: [a])) id ".json"
  writeFile textFileName (show entity)
  writeFile jsonFileName (showJson entity)

showJson :: (ToJSON a) => a -> String
showJson = unpack . toStrict . toLazyText . encodeToTextBuilder . toJSON


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
