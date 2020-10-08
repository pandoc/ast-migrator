{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : Text.Pandoc.AST.V1_21.Up
Copyright   : © 2020 Albert Krewinkel
SPDX-License-Identifier: BSD-3-Clause

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Migrate upwards from V1.21 to V1.22.
-}
module Text.Pandoc.AST.V1_21.Up
  ( migrateUpToV1_22
  , migrateDown
  ) where

import Text.Pandoc.AST.V1_21.Definition
import qualified Text.Pandoc.AST.V1_22.Definition as V1_22
import qualified Data.Map as M

migrateDown :: Pandoc -> V1_22.Pandoc
migrateDown = migrateUpToV1_22

migrateUpToV1_22 :: Pandoc -> V1_22.Pandoc
migrateUpToV1_22 (Pandoc meta blocks) =
  V1_22.Pandoc (migrateMeta meta) (map migrateBlock blocks)

migrateMeta :: Meta -> V1_22.Meta
migrateMeta = V1_22.Meta . M.map migrateMetaValue . unMeta

migrateBlock :: Block -> V1_22.Block
migrateBlock = \case
  BlockQuote blks            -> V1_22.BlockQuote $ migrateBlocks blks
  BulletList items           -> V1_22.BulletList $ migrateItems items
  CodeBlock attr text        -> V1_22.CodeBlock attr text
  DefinitionList defItems    -> V1_22.DefinitionList
                                      $ map migrateDefItem defItems
  Div attr blks              -> V1_22.Div attr $ migrateBlocks blks
  Header lvl attr inlns      -> V1_22.Header lvl attr
                                      $ migrateInlines inlns
  HorizontalRule             -> V1_22.HorizontalRule
  LineBlock lines'           -> V1_22.LineBlock $ map migrateInlines lines'
  Null                       -> V1_22.Null
  OrderedList listAttr items -> V1_22.OrderedList
                                      (migrateListAttributes listAttr)
                                      (migrateItems items)
  Para inlns                 -> V1_22.Para $ migrateInlines inlns
  Plain inlns                -> V1_22.Plain $ migrateInlines inlns
  RawBlock f text            -> V1_22.RawBlock (migrateFormat f) text
  Table as capt col th tb tf -> V1_22.Table as
                                      (migrateCaption capt)
                                      (map migrateColSpec col)
                                      (migrateTableHead th)
                                      (map migrateTableBody tb)
                                      (migrateTableFoot tf)
  where
    migrateItems = map migrateBlocks
    migrateDefItem (def, items) = (migrateInlines def, migrateItems items)

migrateBlocks :: [Block] -> [V1_22.Block]
migrateBlocks = map migrateBlock

migrateInlines :: [Inline] -> [V1_22.Inline]
migrateInlines = map migrateInline

migrateMetaValue :: MetaValue -> V1_22.MetaValue
migrateMetaValue = \case
  MetaBlocks blocks   -> V1_22.MetaBlocks $ map migrateBlock blocks
  MetaBool b          -> V1_22.MetaBool b
  MetaInlines inlines -> V1_22.MetaInlines $ map migrateInline inlines
  MetaList vs         -> V1_22.MetaList $ map migrateMetaValue vs
  MetaMap metamap     -> V1_22.MetaMap $ M.map migrateMetaValue metamap
  MetaString s        -> V1_22.MetaString s

migrateAlignment :: Alignment -> V1_22.Alignment
migrateAlignment =  \case
  AlignLeft    -> V1_22.AlignLeft
  AlignRight   -> V1_22.AlignRight
  AlignCenter  -> V1_22.AlignCenter
  AlignDefault -> V1_22.AlignDefault

migrateColSpec :: ColSpec -> V1_22.ColSpec
migrateColSpec (align, colwidth) =
  let colwidth' = case colwidth of
                    ColWidth d      -> V1_22.ColWidth d
                    ColWidthDefault -> V1_22.ColWidthDefault
  in (migrateAlignment align, colwidth')

migrateCaption :: Caption -> V1_22.Caption
migrateCaption (Caption short long) =
  V1_22.Caption (fmap migrateInlines short) (migrateBlocks long)

migrateTableBody :: TableBody -> V1_22.TableBody
migrateTableBody (TableBody attr (RowHeadColumns rhc) intHead rows) =
  V1_22.TableBody attr
    (V1_22.RowHeadColumns rhc)
    (map migrateRow intHead)
    (map migrateRow rows)

migrateTableFoot :: TableFoot -> V1_22.TableFoot
migrateTableFoot (TableFoot attr rows) =
  V1_22.TableFoot attr (map migrateRow rows)

migrateTableHead :: TableHead -> V1_22.TableHead
migrateTableHead (TableHead attr rows) =
  V1_22.TableHead attr (map migrateRow rows)

migrateRow :: Row -> V1_22.Row
migrateRow (Row attr cells) =
  V1_22.Row attr (map migrateCell cells)

migrateCell :: Cell -> V1_22.Cell
migrateCell (Cell attr align (RowSpan rs) (ColSpan cs) blks) =
  V1_22.Cell
    attr
    (migrateAlignment align)
    (V1_22.RowSpan rs)
    (V1_22.ColSpan cs)
    (migrateBlocks blks)

migrateListNumberDelim :: ListNumberDelim -> V1_22.ListNumberDelim
migrateListNumberDelim = \case
  DefaultDelim -> V1_22.DefaultDelim
  Period       -> V1_22.Period
  OneParen     -> V1_22.OneParen
  TwoParens    -> V1_22.TwoParens

migrateListNumberStyle :: ListNumberStyle -> V1_22.ListNumberStyle
migrateListNumberStyle = \case
  DefaultStyle -> V1_22.DefaultStyle
  Example      -> V1_22.Example
  Decimal      -> V1_22.Decimal
  LowerRoman   -> V1_22.LowerRoman
  UpperRoman   -> V1_22.UpperRoman
  LowerAlpha   -> V1_22.LowerAlpha
  UpperAlpha   -> V1_22.UpperAlpha

migrateListAttributes :: ListAttributes -> V1_22.ListAttributes
migrateListAttributes (n, style, delim) =
  (n, migrateListNumberStyle style, migrateListNumberDelim delim)

migrateFormat :: Format -> V1_22.Format
migrateFormat (Format f) = V1_22.Format f

migrateQuoteType :: QuoteType -> V1_22.QuoteType
migrateQuoteType = \case
  SingleQuote -> V1_22.SingleQuote
  DoubleQuote -> V1_22.DoubleQuote

migrateMathType :: MathType -> V1_22.MathType
migrateMathType = \case
  DisplayMath -> V1_22.DisplayMath
  InlineMath -> V1_22.InlineMath

migrateInline :: Inline -> V1_22.Inline
migrateInline = \case
  Cite citations  inlns -> V1_22.Cite (migrateCitations citations) $ migrateInlines inlns
  Code attr text        -> V1_22.Code attr text
  Emph inlns            -> V1_22.Emph $ migrateInlines inlns
  Image attr inlns tgt  -> V1_22.Image attr (migrateInlines inlns) tgt
  LineBreak             -> V1_22.LineBreak
  Link attr inlns tgt   -> V1_22.Link attr (migrateInlines inlns) tgt
  Math mathType text    -> V1_22.Math (migrateMathType mathType) text
  Note blks             -> V1_22.Note $ migrateBlocks blks
  Quoted qtype inlns    -> V1_22.Quoted (migrateQuoteType qtype) $ migrateInlines inlns
  RawInline f text      -> V1_22.RawInline (migrateFormat f) text
  SmallCaps inlns       -> V1_22.SmallCaps $ migrateInlines inlns
  SoftBreak             -> V1_22.SoftBreak
  Space                 -> V1_22.Space
  Span attr inlns       -> V1_22.Span attr $ migrateInlines inlns
  Str text              -> V1_22.Str text
  Strikeout inlns       -> V1_22.Strikeout $ migrateInlines inlns
  Strong inlns          -> V1_22.Strong $ migrateInlines inlns
  Subscript inlns       -> V1_22.Subscript $ migrateInlines inlns
  Superscript inlns     -> V1_22.Superscript $ migrateInlines inlns
  Underline inlns       -> V1_22.Underline $ migrateInlines inlns
  where
    migrateCitations = map migrateCitation

migrateCitation :: Citation -> V1_22.Citation
migrateCitation citation = V1_22.Citation
  (citationId citation)
  (map migrateInline $ citationPrefix citation)
  (map migrateInline $ citationSuffix citation)
  (migrateCitationMode $ citationMode citation)
  (citationNoteNum citation)
  (citationHash citation)

migrateCitationMode :: CitationMode -> V1_22.CitationMode
migrateCitationMode = \case
  AuthorInText   -> V1_22.AuthorInText
  NormalCitation -> V1_22.NormalCitation
  SuppressAuthor -> V1_22.SuppressAuthor
