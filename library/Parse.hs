{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Data.Aeson
import Control.Applicative
import Control.Monad (mzero)

data Input = Input { getId :: Int
                   , getUnits :: [InputUnit]
                   , getWidth :: Int
                   , getHeight :: Int
                   , getFilled :: [InputCell]
                   , getSourceLength :: Int
                   , getSourceSeeds :: [Int]
                   }

data InputCell = InputCell { getX :: Int , getY :: Int }

data InputUnit = InputUnit { getMembers :: [InputCell] , getPivot :: InputCell }

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

instance FromJSON InputCell where
  parseJSON (Object v) = InputCell <$>
                         v .: "x" <*>
                         v .: "y"
  parseJSON _ =          mzero

instance FromJSON InputUnit where
  parseJSON (Object v) = InputUnit <$>
                         v .: "members" <*>
                         v .: "pivot"
  parseJSON _ =          mzero
