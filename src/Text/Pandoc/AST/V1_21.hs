{- |
Module      : Text.Pandoc.AST.V1_21
Copyright   : © 2020 Albert Krewinkel
SPDX-License-Identifier: BSD-3-Clause

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Version 1.21 of the pandoc document AST.
-}
module Text.Pandoc.AST.V1_21
  ( module Text.Pandoc.AST.V1_21.Definition
  , migrateDownFromV1_21
  ) where

import Text.Pandoc.AST.V1_21.Definition
import Text.Pandoc.AST.V1_21.Down (migrateDownFromV1_21)
