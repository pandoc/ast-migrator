{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Copyright   : © 2020 Albert Krewinkel
SPDX-License-Identifier: BSD-3-Clause

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Run pandoc JSON migrations.
-}
module Main (main) where

import Prelude hiding (interact)

import Data.ByteString.Lazy (interact)
import Options.Applicative
import Text.Pandoc.AST.Migrator (ASTVersion (..), migrateJSON)

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif

-- | Migration parameters
data Migration = Migration
  { initialVersion :: ASTVersion
  , targetVersion  :: ASTVersion
  }
  deriving (Show)

migration :: Parser Migration
migration = Migration
  <$> option astVersion
      (mconcat
       [ long "from"
       , short 'f'
       , metavar "INITIAL"
       , help "initial pandoc AST version"
       , value maxBound
       , showDefaultWith versionToString
       ])
  <*> option astVersion
      (mconcat
       [ long "to"
       , short 't'
       , metavar "TARGET"
       , help "migration target version"
       , value maxBound
       , showDefaultWith versionToString
       ])
  where
    astVersion :: ReadM ASTVersion
    astVersion = eitherReader eitherASTVersion

    versionToString :: ASTVersion -> String
    versionToString = drop 1 . map (\x -> if x == '_' then '.' else x) . show

eitherASTVersion :: String -> Either String ASTVersion
eitherASTVersion = \case
  "1.20" -> Right V1_20
  "1.21" -> Right V1_21
  "1.22" -> Right V1_22
  x      -> Left ("Unsupported version: " <> x)

main :: IO ()
main = do
  params <- execParser opts
  interact $ migrateJSON (initialVersion params) (targetVersion params)
  where
    opts = info (migration <**> helper) $ mconcat
           [ fullDesc
           , progDesc "Migrate between pandoc AST versions"
           , header "ast-migrator - a pandoc AST compatibility tool"
           ]
