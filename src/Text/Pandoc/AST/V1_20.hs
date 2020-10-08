{- |
Module      : Text.Pandoc.AST.V1_20
Copyright   : © 2020 Albert Krewinkel
SPDX-License-Identifier: BSD-3-Clause

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Version 1.20 of the pandoc document AST.
-}
module Text.Pandoc.AST.V1_20
  ( module Text.Pandoc.AST.V1_20.Definition
  , migrateUpToV1_21
  ) where

import Text.Pandoc.AST.V1_20.Definition
import Text.Pandoc.AST.V1_20.Up (migrateUpToV1_21)
