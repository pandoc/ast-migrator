module Main (main) where

import Prelude hiding (interact)

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy (interact)
import Data.List (find)
import System.Environment (getArgs)
import Text.Pandoc.AST.Migrator

main :: IO ()
main = do
  args <- getArgs
  let direction = case find (== "--up") args of
        Just _ -> Up
        Nothing -> Down
  interact $ case direction of
    Up   -> encode
          . either error migrateUpToV1_21
          . eitherDecode
    Down -> encode
          . either error migrateDownFromV1_21
          . eitherDecode

-- | Direction in which the migration takes place.
data Direction
  = Up
  | Down
