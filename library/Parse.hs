{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Parse ( JSONCell (..)
             , JSONPiece (..)
             , JSON (..)
             , readJSON
             ) where


--------------------------------------------------------------------------------
------------------------------------ Header ------------------------------------
--------------------------------------------------------------------------------


-- Imports
import           Control.Applicative ((<*>))
import           Control.Monad       (mzero)
import qualified Data.ByteString     as BS (readFile)

import           Data.Aeson


--------------------------------------------------------------------------------
------------------------------------ Types -------------------------------------
--------------------------------------------------------------------------------


-- | A cell in JSON
data JSONCell = JSONCell { jsonX :: Int
                         , jsonY :: Int
                         } deriving (Eq, Show, Read)

-- | A piece in JSON
data JSONPiece = JSONPiece { jsonMembers :: [JSONCell]
                           , jsonPivot   :: JSONCell
                           } deriving (Eq, Show, Read)

-- | All the information retrieved from a JSON input file
data JSON = JSON { jsonId           :: Int
                 , jsonPieces       :: [JSONPiece]
                 , jsonWidth        :: Int
                 , jsonHeight       :: Int
                 , jsonFilled       :: [JSONCell]
                 , jsonSourceLength :: Int
                 , jsonSourceSeeds  :: [Int]
                 } deriving (Eq, Show, Read)


--------------------------------------------------------------------------------
---------------------------------- Functions -----------------------------------
--------------------------------------------------------------------------------


-- | Read JSON from a file and strictly decode it into a 'JSON'
readJSON :: FilePath -> IO (Maybe JSON)
readJSON path = decodeStrict' <$> BS.readFile path


--------------------------------------------------------------------------------
---------------------------------- Instances -----------------------------------
--------------------------------------------------------------------------------


-- | Parse a 'JSON'
instance FromJSON JSON where
  parseJSON (Object v) = JSON <$> v .: "id"
                              <*> v .: "units"
                              <*> v .: "width"
                              <*> v .: "height"
                              <*> v .: "filled"
                              <*> v .: "sourceLength"
                              <*> v .: "sourceSeeds"
  parseJSON _          = mzero

-- | Parse a 'JSONCell'
instance FromJSON JSONCell where
  parseJSON (Object v) = JSONCell <$> v .: "x"
                                  <*> v .: "y"
  parseJSON _          = mzero

-- | Parse a 'JSONPiece'
instance FromJSON JSONPiece where
  parseJSON (Object v) = JSONPiece <$> v .: "members"
                                   <*> v .: "pivot"
  parseJSON _          = mzero
