{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Wrapper module for ICFP 2015 entry
module ICFP2015 (module ICFP2015) where

import           CommandLine

-- | Main entry point of application
main :: IO ()
main = do
  opts <- getOptions
  print opts
