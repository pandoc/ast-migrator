{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{- |
Module      : Text.Pandoc.AST.V1_22.Down
Copyright   : © 2020 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Migrate from V1.22 to V1.21.
-}
module Text.Pandoc.AST.V1_22.Down
  ( migrateDownFromV1_22
  , migrateDown
  ) where

import Text.Pandoc.AST.V1_22.Definition
import qualified Text.Pandoc.AST.V1_21.Definition as V1_21
import qualified Data.Map as M

migrateDown :: Pandoc -> V1_21.Pandoc
migrateDown = migrateDownFromV1_22

migrateDownFromV1_22 :: Pandoc -> V1_21.Pandoc
migrateDownFromV1_22 (Pandoc meta blocks) =
  V1_21.Pandoc (migrateMeta meta) (map migrateBlock blocks)

migrateMeta :: Meta -> V1_21.Meta
migrateMeta = V1_21.Meta . M.map migrateMetaValue . unMeta

migrateBlock :: Block -> V1_21.Block
migrateBlock = \case
  BlockQuote blks            -> V1_21.BlockQuote $ migrateBlocks blks
  BulletList items           -> V1_21.BulletList $ migrateItems items
  CodeBlock attr text        -> V1_21.CodeBlock attr text
  DefinitionList defItems    -> V1_21.DefinitionList
                                      $ map migrateDefItem defItems
  Div attr blks              -> V1_21.Div attr $ migrateBlocks blks
  Header lvl attr inlns      -> V1_21.Header lvl attr
                                      $ migrateInlines inlns
  HorizontalRule             -> V1_21.HorizontalRule
  LineBlock lines'           -> V1_21.LineBlock $ map migrateInlines lines'
  Null                       -> V1_21.Null
  OrderedList listAttr items -> V1_21.OrderedList
                                      (migrateListAttributes listAttr)
                                      (migrateItems items)
  Para inlns                 -> V1_21.Para $ migrateInlines inlns
  Plain inlns                -> V1_21.Plain $ migrateInlines inlns
  RawBlock f text            -> V1_21.RawBlock (migrateFormat f) text
  Table as capt col th tb tf -> V1_21.Table as
                                      (migrateCaption capt)
                                      (map migrateColSpec col)
                                      (migrateTableHead th)
                                      (map migrateTableBody tb)
                                      (migrateTableFoot tf)
  where
    migrateItems = map migrateBlocks
    migrateDefItem (def, items) = (migrateInlines def, migrateItems items)

migrateBlocks :: [Block] -> [V1_21.Block]
migrateBlocks = map migrateBlock

migrateInlines :: [Inline] -> [V1_21.Inline]
migrateInlines = map migrateInline

migrateMetaValue :: MetaValue -> V1_21.MetaValue
migrateMetaValue = \case
  MetaBlocks blocks   -> V1_21.MetaBlocks $ map migrateBlock blocks
  MetaBool b          -> V1_21.MetaBool b
  MetaInlines inlines -> V1_21.MetaInlines $ map migrateInline inlines
  MetaList vs         -> V1_21.MetaList $ map migrateMetaValue vs
  MetaMap metamap     -> V1_21.MetaMap $ M.map migrateMetaValue metamap
  MetaString s        -> V1_21.MetaString s

migrateAlignment :: Alignment -> V1_21.Alignment
migrateAlignment =  \case
  AlignLeft    -> V1_21.AlignLeft
  AlignRight   -> V1_21.AlignRight
  AlignCenter  -> V1_21.AlignCenter
  AlignDefault -> V1_21.AlignDefault

migrateColSpec :: ColSpec -> V1_21.ColSpec
migrateColSpec (align, colwidth) =
  let colwidth' = case colwidth of
                    ColWidth d      -> V1_21.ColWidth d
                    ColWidthDefault -> V1_21.ColWidthDefault
  in (migrateAlignment align, colwidth')

migrateCaption :: Caption -> V1_21.Caption
migrateCaption (Caption short long) =
  V1_21.Caption (fmap migrateInlines short) (migrateBlocks long)

migrateTableBody :: TableBody -> V1_21.TableBody
migrateTableBody (TableBody attr (RowHeadColumns rhc) intHead rows) =
  V1_21.TableBody attr
    (V1_21.RowHeadColumns rhc)
    (map migrateRow intHead)
    (map migrateRow rows)

migrateTableFoot :: TableFoot -> V1_21.TableFoot
migrateTableFoot (TableFoot attr rows) =
  V1_21.TableFoot attr (map migrateRow rows)

migrateTableHead :: TableHead -> V1_21.TableHead
migrateTableHead (TableHead attr rows) =
  V1_21.TableHead attr (map migrateRow rows)

migrateRow :: Row -> V1_21.Row
migrateRow (Row attr cells) =
  V1_21.Row attr (map migrateCell cells)

migrateCell :: Cell -> V1_21.Cell
migrateCell (Cell attr align (RowSpan rs) (ColSpan cs) blks) =
  V1_21.Cell
    attr
    (migrateAlignment align)
    (V1_21.RowSpan rs)
    (V1_21.ColSpan cs)
    (migrateBlocks blks)

migrateListNumberDelim :: ListNumberDelim -> V1_21.ListNumberDelim
migrateListNumberDelim = \case
  DefaultDelim -> V1_21.DefaultDelim
  Period       -> V1_21.Period
  OneParen     -> V1_21.OneParen
  TwoParens    -> V1_21.TwoParens

migrateListNumberStyle :: ListNumberStyle -> V1_21.ListNumberStyle
migrateListNumberStyle = \case
  DefaultStyle -> V1_21.DefaultStyle
  Example      -> V1_21.Example
  Decimal      -> V1_21.Decimal
  LowerRoman   -> V1_21.LowerRoman
  UpperRoman   -> V1_21.UpperRoman
  LowerAlpha   -> V1_21.LowerAlpha
  UpperAlpha   -> V1_21.UpperAlpha

migrateListAttributes :: ListAttributes -> V1_21.ListAttributes
migrateListAttributes (n, style, delim) =
  (n, migrateListNumberStyle style, migrateListNumberDelim delim)

migrateFormat :: Format -> V1_21.Format
migrateFormat (Format f) = V1_21.Format f

migrateQuoteType :: QuoteType -> V1_21.QuoteType
migrateQuoteType = \case
  SingleQuote -> V1_21.SingleQuote
  DoubleQuote -> V1_21.DoubleQuote

migrateMathType :: MathType -> V1_21.MathType
migrateMathType = \case
  DisplayMath -> V1_21.DisplayMath
  InlineMath -> V1_21.InlineMath

migrateInline :: Inline -> V1_21.Inline
migrateInline = \case
  Cite citations  inlns -> V1_21.Cite (migrateCitations citations) $ migrateInlines inlns
  Code attr text        -> V1_21.Code attr text
  Emph inlns            -> V1_21.Emph $ migrateInlines inlns
  Image attr inlns tgt  -> V1_21.Image attr (migrateInlines inlns) tgt
  LineBreak             -> V1_21.LineBreak
  Link attr inlns tgt   -> V1_21.Link attr (migrateInlines inlns) tgt
  Math mathType text    -> V1_21.Math (migrateMathType mathType) text
  Note blks             -> V1_21.Note $ migrateBlocks blks
  Quoted qtype inlns    -> V1_21.Quoted (migrateQuoteType qtype) $ migrateInlines inlns
  RawInline f text      -> V1_21.RawInline (migrateFormat f) text
  SmallCaps inlns       -> V1_21.SmallCaps $ migrateInlines inlns
  SoftBreak             -> V1_21.SoftBreak
  Space                 -> V1_21.Space
  Span attr inlns       -> V1_21.Span attr $ migrateInlines inlns
  Str text              -> V1_21.Str text
  Strikeout inlns       -> V1_21.Strikeout $ migrateInlines inlns
  Strong inlns          -> V1_21.Strong $ migrateInlines inlns
  Subscript inlns       -> V1_21.Subscript $ migrateInlines inlns
  Superscript inlns     -> V1_21.Superscript $ migrateInlines inlns
  Underline inlns       -> V1_21.Underline $ migrateInlines inlns
  where
    migrateCitations = map migrateCitation

migrateCitation :: Citation -> V1_21.Citation
migrateCitation citation = V1_21.Citation
  (citationId citation)
  (map migrateInline $ citationPrefix citation)
  (map migrateInline $ citationSuffix citation)
  (migrateCitationMode $ citationMode citation)
  (citationNoteNum citation)
  (citationHash citation)

migrateCitationMode :: CitationMode -> V1_21.CitationMode
migrateCitationMode = \case
  AuthorInText   -> V1_21.AuthorInText
  NormalCitation -> V1_21.NormalCitation
  SuppressAuthor -> V1_21.SuppressAuthor
