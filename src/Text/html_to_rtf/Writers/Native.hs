{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Writers.Native ( writeNative )
where
import Text.Pandoc.Options ( WriterOptions(..), WrapOption(..) )
import Data.List ( intersperse )
import Text.Pandoc.Definition
import Text.Pandoc.Pretty

prettyList :: [Doc] -> Doc
prettyList ds =
  "[" <> (cat $ intersperse (cr <> ",") $ map (nest 1) ds) <> "]"

-- | Prettyprint Pandoc block element.
prettyBlock :: Block -> Doc
prettyBlock (BlockQuote blocks) =
  "BlockQuote" $$ prettyList (map prettyBlock blocks)
prettyBlock (OrderedList attribs blockLists) =
  "OrderedList" <> space <> text (show attribs) $$
  (prettyList $ map (prettyList . map prettyBlock) blockLists)
prettyBlock (BulletList blockLists) =
  "BulletList" $$
  (prettyList $ map (prettyList . map prettyBlock) blockLists)
prettyBlock (DefinitionList items) = "DefinitionList" $$
  (prettyList $ map deflistitem items)
    where deflistitem (term, defs) = "(" <> text (show term) <> "," <> cr <>
           nest 1 (prettyList $ map (prettyList . map prettyBlock) defs) <> ")"
prettyBlock (Table caption aligns widths header rows) =
  "Table " <> text (show caption) <> " " <> text (show aligns) <> " " <>
  text (show widths) $$
  prettyRow header $$
  prettyList (map prettyRow rows)
    where prettyRow cols = prettyList (map (prettyList . map prettyBlock) cols)
prettyBlock (Div attr blocks) =
  text ("Div " <> show attr) $$ prettyList (map prettyBlock blocks)
prettyBlock block = text $ show block

-- | Prettyprint Pandoc document.
writeNative :: WriterOptions -> Pandoc -> String
writeNative opts (Pandoc meta blocks) =
  let colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      withHead = if writerStandalone opts
                    then \bs -> text ("Pandoc (" ++ show meta ++ ")") $$
                                  bs $$ cr
                    else id
  in  render colwidth $ withHead $ prettyList $ map prettyBlock blocks
