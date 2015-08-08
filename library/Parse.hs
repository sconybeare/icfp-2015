{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)

data Input = Input { id :: Int
                   , units :: [Piece]
                   , width :: Int
                   , height :: Int
                   , filled :: [Cell]
                   , sourceLength :: Int
                   , sourceSeeds :: [Int]
                   }

data Cell = Cell { x :: Int , y :: Int }

data Piece = Piece { members :: [Cell] , pivot :: Cell }

instance FromJSON Input where
  parseJSON (Object v) = Input <$>
                         v .: "id" <*>
                         v .: "units" <*>
                         v .: "width" <*>
                         v .: "height" <*>
                         v .: "filled" <*>
                         v .: "sourceLength" <*>
                         v .: "sourceSeeds"
  parseJSON _ =          mzero

instance FromJSON Cell where
  parseJSON (Object v) = Cell <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _ =          mzero

instance FromJSON Piece where
  parseJSON (Object v) = Piece <$>
                         v .: "members" <*>
                         v .: "pivot"
  parseJSON _ =          mzero
