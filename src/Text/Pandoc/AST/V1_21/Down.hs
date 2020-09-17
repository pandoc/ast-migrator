{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Text.Pandoc.AST.V1_21.Down
Copyright   : © 2020 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <albert@zeitkraut.de>
Stability   : alpha
Portability : portable

Version 1.20 of the pandoc document AST.
-}
module Text.Pandoc.AST.V1_21.Down
  ( migrateDownFromV1_21
  , migrateDown
  ) where

import Data.List (intercalate, intersperse)
import Text.Pandoc.AST.V1_21.Definition
import qualified Text.Pandoc.AST.V1_21.Builder as Builder
import qualified Text.Pandoc.AST.V1_20.Definition as V1_20
import qualified Data.Map as M

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup(..))
#endif

migrateDown :: Pandoc -> V1_20.Pandoc
migrateDown = migrateDownFromV1_21

migrateDownFromV1_21 :: Pandoc -> V1_20.Pandoc
migrateDownFromV1_21 (Pandoc meta blocks) =
  V1_20.Pandoc (migrateMeta meta) (map migrateBlock blocks)

migrateMeta :: Meta -> V1_20.Meta
migrateMeta = V1_20.Meta . M.map migrateMetaValue . unMeta

migrateBlock :: Block -> V1_20.Block
migrateBlock = \case
  BlockQuote blks            -> V1_20.BlockQuote $ migrateBlocks blks
  BulletList items           -> V1_20.BulletList $ migrateItems items
  CodeBlock attr text        -> V1_20.CodeBlock attr text
  DefinitionList defItems    -> V1_20.DefinitionList
                                      $ map migrateDefItem defItems
  Div attr blks              -> V1_20.Div attr $ migrateBlocks blks
  Header lvl attr inlns      -> V1_20.Header lvl attr
                                      $ migrateInlines inlns
  HorizontalRule             -> V1_20.HorizontalRule
  LineBlock lines'           -> V1_20.LineBlock $ map migrateInlines lines'
  Null                       -> V1_20.Null
  OrderedList listAttr items -> V1_20.OrderedList
                                      (migrateListAttributes listAttr)
                                      (migrateItems items)
  Para inlns                 -> V1_20.Para $ migrateInlines inlns
  Plain inlns                -> V1_20.Plain $ migrateInlines inlns
  RawBlock f text            -> V1_20.RawBlock (migrateFormat f) text
  Table _ capt col th tb tf  -> migrateTable capt col th tb tf
  where
    migrateBlocks = map migrateBlock
    migrateInlines = map migrateInline
    migrateItems = map migrateBlocks
    migrateDefItem (def, items) = (migrateInlines def, migrateItems items)

migrateMetaValue :: MetaValue -> V1_20.MetaValue
migrateMetaValue = \case
  MetaBlocks blocks   -> V1_20.MetaBlocks $ map migrateBlock blocks
  MetaBool b          -> V1_20.MetaBool b
  MetaInlines inlines -> V1_20.MetaInlines $ map migrateInline inlines
  MetaList vs         -> V1_20.MetaList $ map migrateMetaValue vs
  MetaMap metamap     -> V1_20.MetaMap $ M.map migrateMetaValue metamap
  MetaString s        -> V1_20.MetaString s

migrateAlignment :: Alignment -> V1_20.Alignment
migrateAlignment =  \case
  AlignLeft    -> V1_20.AlignLeft
  AlignRight   -> V1_20.AlignRight
  AlignCenter  -> V1_20.AlignCenter
  AlignDefault -> V1_20.AlignDefault

migrateListNumberDelim :: ListNumberDelim -> V1_20.ListNumberDelim
migrateListNumberDelim = \case
  DefaultDelim -> V1_20.DefaultDelim
  Period       -> V1_20.Period
  OneParen     -> V1_20.OneParen
  TwoParens    -> V1_20.TwoParens

migrateListNumberStyle :: ListNumberStyle -> V1_20.ListNumberStyle
migrateListNumberStyle = \case
  DefaultStyle -> V1_20.DefaultStyle
  Example      -> V1_20.Example
  Decimal      -> V1_20.Decimal
  LowerRoman   -> V1_20.LowerRoman
  UpperRoman   -> V1_20.UpperRoman
  LowerAlpha   -> V1_20.LowerAlpha
  UpperAlpha   -> V1_20.UpperAlpha

migrateListAttributes :: ListAttributes -> V1_20.ListAttributes
migrateListAttributes (n, style, delim) =
  (n, migrateListNumberStyle style, migrateListNumberDelim delim)

migrateFormat :: Format -> V1_20.Format
migrateFormat (Format f) = V1_20.Format f

migrateQuoteType :: QuoteType -> V1_20.QuoteType
migrateQuoteType = \case
  SingleQuote -> V1_20.SingleQuote
  DoubleQuote -> V1_20.DoubleQuote

migrateMathType :: MathType -> V1_20.MathType
migrateMathType = \case
  DisplayMath -> V1_20.DisplayMath
  InlineMath -> V1_20.InlineMath

migrateInline :: Inline -> V1_20.Inline
migrateInline = \case
  Cite citations  inlns -> V1_20.Cite (migrateCitations citations) $ migrateInlines inlns
  Code attr text        -> V1_20.Code attr text
  Emph inlns            -> V1_20.Emph $ migrateInlines inlns
  Image attr inlns tgt  -> V1_20.Image attr (migrateInlines inlns) tgt
  LineBreak             -> V1_20.LineBreak
  Link attr inlns tgt   -> V1_20.Link attr (migrateInlines inlns) tgt
  Math mathType text    -> V1_20.Math (migrateMathType mathType) text
  Note blks             -> V1_20.Note $ migrateBlocks blks
  Quoted qtype inlns    -> V1_20.Quoted (migrateQuoteType qtype) $ migrateInlines inlns
  RawInline f text      -> V1_20.RawInline (migrateFormat f) text
  SmallCaps inlns       -> V1_20.SmallCaps $ migrateInlines inlns
  SoftBreak             -> V1_20.SoftBreak
  Space                 -> V1_20.Space
  Span attr inlns       -> V1_20.Span attr $ migrateInlines inlns
  Str text              -> V1_20.Str text
  Strikeout inlns       -> V1_20.Strikeout $ migrateInlines inlns
  Strong inlns          -> V1_20.Strong $ migrateInlines inlns
  Subscript inlns       -> V1_20.Subscript $ migrateInlines inlns
  Superscript inlns     -> V1_20.Superscript $ migrateInlines inlns
  Underline inlns       -> V1_20.Span ("", ["underlined"], [])
                           $ migrateInlines inlns
  where
    migrateBlocks = map migrateBlock
    migrateInlines = map migrateInline
    migrateCitations = map migrateCitation

migrateCitation :: Citation -> V1_20.Citation
migrateCitation citation = V1_20.Citation
  (citationId citation)
  (map migrateInline $ citationPrefix citation)
  (map migrateInline $ citationSuffix citation)
  (migrateCitationMode $ citationMode citation)
  (citationNoteNum citation)
  (citationHash citation)

migrateCitationMode :: CitationMode -> V1_20.CitationMode
migrateCitationMode = \case
  AuthorInText   -> V1_20.AuthorInText
  NormalCitation -> V1_20.NormalCitation
  SuppressAuthor -> V1_20.SuppressAuthor

-- | Convert the relevant components of a new-style table (with block
-- caption, row headers, row and column spans, and so on) to those of
-- an old-style table (inline caption, table head with one row, no
-- foot, and so on). Cells with a 'RowSpan' and 'ColSpan' of @(h, w)@
-- will be cut up into @h * w@ cells of dimension @(1, 1)@, with the
-- content placed in the upper-left corner.
migrateTable :: Caption
             -> [ColSpec]
             -> TableHead
             -> [TableBody]
             -> TableFoot
             -> V1_20.Block
migrateTable (Caption _ cbody) specs thead tbodies tfoot
  = V1_20.Table (map migrateInline $ Builder.toList cbody')
                (map migrateAlignment aligns)
                widths
                (map (map migrateBlock) th')
                (map (map (map migrateBlock)) tb')
  where
    numcols = length specs
    (aligns, mwidths) = unzip specs
    fromWidth (ColWidth w) | w > 0 = w
    fromWidth _                    = 0
    widths = map fromWidth mwidths
    unRow (Row _ x) = x
    unBody (TableBody _ _ hd bd) = hd <> bd
    unBodies = concatMap unBody

    TableHead _ th = Builder.normalizeTableHead numcols thead
    tb = map (Builder.normalizeTableBody numcols) tbodies
    TableFoot _ tf = Builder.normalizeTableFoot numcols tfoot

    cbody' = blocksToInlines cbody

    (th', tb') = case th of
      r:rs -> let (pendingPieces, r') = placeCutCells [] $ unRow r
                  rs' = cutRows pendingPieces $ rs <> unBodies tb <> tf
              in (r', rs')
      []    -> ([], cutRows [] $ unBodies tb <> tf)

    -- Adapted from placeRowSection in Builders. There is probably a
    -- more abstract foldRowSection that unifies them both.
    placeCutCells pendingPieces cells
      -- If there are any pending pieces for a column, add
      -- them. Pending pieces have preference over cells due to grid
      -- layout rules.
      | (p:ps):pendingPieces' <- pendingPieces
      = let (pendingPieces'', rowPieces) = placeCutCells pendingPieces' cells
        in (ps : pendingPieces'', p : rowPieces)
      -- Otherwise cut up a cell on the row and deal with its pieces.
      | c:cells' <- cells
      = let (h, w, cBody) = getComponents c
            cRowPieces = cBody : replicate (w - 1) mempty
            cPendingPieces = replicate w $ replicate (h - 1) mempty
            pendingPieces' = dropWhile null pendingPieces
            (pendingPieces'', rowPieces) = placeCutCells pendingPieces' cells'
        in (cPendingPieces <> pendingPieces'', cRowPieces <> rowPieces)
      | otherwise = ([], [])

    cutRows pendingPieces (r:rs)
      = let (pendingPieces', r') = placeCutCells pendingPieces $ unRow r
            rs' = cutRows pendingPieces' rs
        in r' : rs'
    cutRows _ [] = []

    getComponents (Cell _ _ (RowSpan h) (ColSpan w) body)
      = (h, w, body)

blockToInlines :: Block -> Builder.Inlines
blockToInlines (Plain ils) = Builder.fromList ils
blockToInlines (Para ils) = Builder.fromList ils
blockToInlines (LineBlock lns) = Builder.fromList $ intercalate [LineBreak] lns
blockToInlines (CodeBlock attr str) = Builder.codeWith attr str
blockToInlines (RawBlock (Format fmt) str) = Builder.rawInline fmt str
blockToInlines (BlockQuote blks) = blocksToInlines blks
blockToInlines (OrderedList _ blkslst) =
  mconcat $ map blocksToInlines blkslst
blockToInlines (BulletList blkslst) =
  mconcat $ map blocksToInlines blkslst
blockToInlines (DefinitionList pairslst) =
  mconcat $ map f pairslst
  where
    f (ils, blkslst) = Builder.fromList ils <> Builder.str ":" <> Builder.space <>
      mconcat (map blocksToInlines blkslst)
blockToInlines (Header _ _  ils) = Builder.fromList ils
blockToInlines HorizontalRule = mempty
blockToInlines (Table _ _ _ (TableHead _ hbd) bodies (TableFoot _ fbd)) =
  mconcat $ intersperse Builder.linebreak $
    map (mconcat . map blocksToInlines) (plainRowBody <$> hbd <> unTableBodies bodies <> fbd)
  where
    plainRowBody (Row _ body) = cellBody <$> body
    cellBody (Cell _ _ _ _ body) = body
    unTableBody (TableBody _ _ hd bd) = hd <> bd
    unTableBodies = concatMap unTableBody
blockToInlines (Div _ blks) = blocksToInlines blks
blockToInlines Null = mempty

blocksToInlines :: [Block] -> Builder.Inlines
blocksToInlines = mconcat .
  intersperse sep . map blockToInlines
  where sep = Builder.space <> Builder.str "¶" <> Builder.space
