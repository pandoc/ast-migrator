{- |
Module      : Text.Pandoc.AST.Migrator
Copyright   : © 2020 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Migrate from or to older AST versions.
-}
module Text.Pandoc.AST.Migrator
  ( migrateUpToV1_21
  , migrateUpToV1_22
  , migrateDownFromV1_21
  , migrateDownFromV1_22
  ) where

import Text.Pandoc.AST.V1_20.Up (migrateUpToV1_21)
import Text.Pandoc.AST.V1_21.Down (migrateDownFromV1_21)
import Text.Pandoc.AST.V1_21.Up (migrateUpToV1_22)
import Text.Pandoc.AST.V1_22.Down (migrateDownFromV1_22)
