{- |
Module      : Text.Pandoc.AST.Migrator
Copyright   : © 2020 Albert Krewinkel
SPDX-License-Identifier: BSD-3-Clause

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Migrate from or to older API versions.
-}
module Text.Pandoc.AST.Migrator
  ( ASTVersion (..)
  , migrateJSON
  , migrateJSONWith
    -- * migration steps
  , migrateUpToV1_21
  , migrateUpToV1_22
  , migrateDownFromV1_21
  , migrateDownFromV1_22
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy (ByteString)
import Text.Pandoc.AST.V1_20.Up (migrateUpToV1_21)
import Text.Pandoc.AST.V1_21.Down (migrateDownFromV1_21)
import Text.Pandoc.AST.V1_21.Up (migrateUpToV1_22)
import Text.Pandoc.AST.V1_22.Down (migrateDownFromV1_22)

-- | Supported pandoc API versions.
data ASTVersion
  = V1_20  -- ^ Version 1.20
  | V1_21  -- ^ Version 1.21
  | V1_22  -- ^ Version 1.22
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

migrateJSON :: ASTVersion -- ^ initial version
            -> ASTVersion -- ^ target version
            -> ByteString -- ^ input JSON
            -> ByteString -- ^ migrated JSON
migrateJSON from to =
  case (from, to) of
    (V1_20, V1_20) -> id
    (V1_20, V1_21) -> migrateJSONWith migrateUpToV1_21
    (V1_20, V1_22) -> migrateJSONWith (migrateUpToV1_22 . migrateUpToV1_21)
    (V1_21, V1_21) -> id
    (V1_21, V1_22) -> migrateJSONWith migrateUpToV1_22
    (V1_22, V1_22) -> id
    (V1_22, V1_21) -> migrateJSONWith migrateDownFromV1_22
    (V1_22, V1_20) -> migrateJSONWith (migrateDownFromV1_21 . migrateDownFromV1_22)
    (V1_21, V1_20) -> migrateJSONWith migrateDownFromV1_21

migrateJSONWith :: (FromJSON a, ToJSON b)
                => (a -> b)    -- ^ Migration function
                -> ByteString  -- ^ Input JSON
                -> ByteString  -- ^ Migrated JSON
migrateJSONWith migrator =
  encode . either error migrator . eitherDecode
