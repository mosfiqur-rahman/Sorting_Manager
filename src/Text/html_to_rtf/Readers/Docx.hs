{-# LANGUAGE PatternGuards, OverloadedStrings #-}



module Text.Pandoc.Readers.Docx
       ( readDocxWithWarnings
       , readDocx
       ) where

import Codec.Archive.Zip
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Builder
import Text.Pandoc.Walk
import Text.Pandoc.Readers.Docx.Parse
import Text.Pandoc.Readers.Docx.Lists
import Text.Pandoc.Readers.Docx.Combine
import Text.Pandoc.Shared
import Text.Pandoc.MediaBag (insertMedia, MediaBag)
import Data.List (delete, (\\), intersect)
import Text.TeXMath (writeTeX)
import Data.Default (Default)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.State
import Data.Sequence (ViewL(..), viewl)
import qualified Data.Sequence as Seq (null)

import Text.Pandoc.Error
import Text.Pandoc.Compat.Except

readDocxWithWarnings :: ReaderOptions
                     -> B.ByteString
                     -> Either PandocError (Pandoc, MediaBag, [String])
readDocxWithWarnings opts bytes =
  case archiveToDocxWithWarnings (toArchive bytes) of
    Right (docx, warnings) -> do
      (meta, blks, mediaBag) <- docxToOutput opts docx
      return (Pandoc meta blks, mediaBag, warnings)
    Left _   -> Left (ParseFailure "couldn't parse docx file")

readDocx :: ReaderOptions
         -> B.ByteString
         -> Either PandocError (Pandoc, MediaBag)
readDocx opts bytes = do
  (pandoc, mediaBag, _) <- readDocxWithWarnings opts bytes
  return (pandoc, mediaBag)

data DState = DState { docxAnchorMap :: M.Map String String
                     , docxMediaBag      :: MediaBag
                     , docxDropCap       :: Inlines
                     }

instance Default DState where
  def = DState { docxAnchorMap = M.empty
               , docxMediaBag  = mempty
               , docxDropCap   = mempty
               }

data DEnv = DEnv { docxOptions  :: ReaderOptions
                 , docxInHeaderBlock :: Bool }

instance Default DEnv where
  def = DEnv def False

type DocxContext = ExceptT PandocError (ReaderT DEnv (State DState))

evalDocxContext :: DocxContext a -> DEnv -> DState -> Either PandocError a
evalDocxContext ctx env st = flip evalState st . flip runReaderT env . runExceptT $ ctx

-- This is empty, but we put it in for future-proofing.
spansToKeep :: [String]
spansToKeep = []

divsToKeep :: [String]
divsToKeep = ["list-item", "Definition", "DefinitionTerm"]

metaStyles :: M.Map String String
metaStyles = M.fromList [ ("Title", "title")
                        , ("Subtitle", "subtitle")
                        , ("Author", "author")
                        , ("Date", "date")
                        , ("Abstract", "abstract")]

sepBodyParts :: [BodyPart] -> ([BodyPart], [BodyPart])
sepBodyParts = span (\bp -> (isMetaPar bp || isEmptyPar bp))

isMetaPar :: BodyPart -> Bool
isMetaPar (Paragraph pPr _) =
  not $ null $ intersect (pStyle pPr) (M.keys metaStyles)
isMetaPar _ = False

isEmptyPar :: BodyPart -> Bool
isEmptyPar (Paragraph _ parParts) =
  all isEmptyParPart parParts
  where
    isEmptyParPart (PlainRun (Run _ runElems)) = all isEmptyElem runElems
    isEmptyParPart _ = False
    isEmptyElem (TextRun s) = trim s == ""
    isEmptyElem _           = True
isEmptyPar _ = False

bodyPartsToMeta' :: [BodyPart] -> DocxContext (M.Map String MetaValue)
bodyPartsToMeta' [] = return M.empty
bodyPartsToMeta' (bp : bps)
  | (Paragraph pPr parParts) <- bp
  , (c : _)<- intersect (pStyle pPr) (M.keys metaStyles)
  , (Just metaField) <- M.lookup c metaStyles = do
    inlines <- smushInlines <$> mapM parPartToInlines parParts
    remaining <- bodyPartsToMeta' bps
    let
      f (MetaInlines ils) (MetaInlines ils') = MetaBlocks [Para ils, Para ils']
      f (MetaInlines ils) (MetaBlocks blks) = MetaBlocks ((Para ils) : blks)
      f m (MetaList mv) = MetaList (m : mv)
      f m n             = MetaList [m, n]
    return $ M.insertWith f metaField (MetaInlines (toList inlines)) remaining
bodyPartsToMeta' (_ : bps) = bodyPartsToMeta' bps

bodyPartsToMeta :: [BodyPart] -> DocxContext Meta
bodyPartsToMeta bps = do
  mp <- bodyPartsToMeta' bps
  let mp' =
        case M.lookup "author" mp of
          Just mv -> M.insert "author" (fixAuthors mv) mp
          Nothing -> mp
  return $ Meta mp'

fixAuthors :: MetaValue -> MetaValue
fixAuthors (MetaBlocks blks) =
  MetaList $ map g $ filter f blks
    where f (Para _) = True
          f _          = False
          g (Para ils) = MetaInlines ils
          g _          = MetaInlines []
fixAuthors mv = mv

codeStyles :: [String]
codeStyles = ["VerbatimChar"]

codeDivs :: [String]
codeDivs = ["SourceCode"]

runElemToInlines :: RunElem -> Inlines
runElemToInlines (TextRun s) = text s
runElemToInlines (LnBrk) = linebreak
runElemToInlines (Tab) = space
runElemToInlines (SoftHyphen) = text "\xad"
runElemToInlines (NoBreakHyphen) = text "\x2011"

runElemToString :: RunElem -> String
runElemToString (TextRun s) = s
runElemToString (LnBrk) = ['\n']
runElemToString (Tab) = ['\t']
runElemToString (SoftHyphen) = ['\xad']
runElemToString (NoBreakHyphen) = ['\x2011']

runToString :: Run -> String
runToString (Run _ runElems) = concatMap runElemToString runElems
runToString _ = ""

parPartToString :: ParPart -> String
parPartToString (PlainRun run) = runToString run
parPartToString (InternalHyperLink _ runs) = concatMap runToString runs
parPartToString (ExternalHyperLink _ runs) = concatMap runToString runs
parPartToString _ = ""

blacklistedCharStyles :: [String]
blacklistedCharStyles = ["Hyperlink"]

resolveDependentRunStyle :: RunStyle -> RunStyle
resolveDependentRunStyle rPr
  | Just (s, _)  <- rStyle rPr, s `elem` blacklistedCharStyles =
    rPr
  | Just (_, cs) <- rStyle rPr =
      let rPr' = resolveDependentRunStyle cs
      in
       RunStyle { isBold = case isBold rPr of
                     Just bool -> Just bool
                     Nothing   -> isBold rPr'
                , isItalic = case isItalic rPr of
                     Just bool -> Just bool
                     Nothing   -> isItalic rPr'
                , isSmallCaps = case isSmallCaps rPr of
                     Just bool -> Just bool
                     Nothing   -> isSmallCaps rPr'
                , isStrike = case isStrike rPr of
                     Just bool -> Just bool
                     Nothing   -> isStrike rPr'
                , rVertAlign = case rVertAlign rPr of
                     Just valign -> Just valign
                     Nothing     -> rVertAlign rPr'
                , rUnderline = case rUnderline rPr of
                     Just ulstyle -> Just ulstyle
                     Nothing      -> rUnderline rPr'
                , rStyle = rStyle rPr }
  | otherwise = rPr

runStyleToTransform :: RunStyle -> (Inlines -> Inlines)
runStyleToTransform rPr
  | Just (s, _) <- rStyle rPr
  , s `elem` spansToKeep =
    let rPr' = rPr{rStyle = Nothing}
    in
     (spanWith ("", [s], [])) . (runStyleToTransform rPr')
  | Just True <- isItalic rPr =
      emph . (runStyleToTransform rPr {isItalic = Nothing})
  | Just True <- isBold rPr =
      strong . (runStyleToTransform rPr {isBold = Nothing})
  | Just True <- isSmallCaps rPr =
      smallcaps . (runStyleToTransform rPr {isSmallCaps = Nothing})
  | Just True <- isStrike rPr =
      strikeout . (runStyleToTransform rPr {isStrike = Nothing})
  | Just SupScrpt <- rVertAlign rPr =
      superscript . (runStyleToTransform rPr {rVertAlign = Nothing})
  | Just SubScrpt <- rVertAlign rPr =
      subscript . (runStyleToTransform rPr {rVertAlign = Nothing})
  | Just "single" <- rUnderline rPr =
      emph . (runStyleToTransform rPr {rUnderline = Nothing})
  | otherwise = id

runToInlines :: Run -> DocxContext Inlines
runToInlines (Run rs runElems)
  | Just (s, _) <- rStyle rs
  , s `elem` codeStyles =
    let rPr = resolveDependentRunStyle rs
        codeString = code $ concatMap runElemToString runElems
    in
     return $ case rVertAlign rPr of
     Just SupScrpt -> superscript codeString
     Just SubScrpt -> subscript codeString
     _             -> codeString
  | otherwise = do
    let ils = smushInlines (map runElemToInlines runElems)
    return $ (runStyleToTransform $ resolveDependentRunStyle rs) ils
runToInlines (Footnote bps) = do
  blksList <- smushBlocks <$> (mapM bodyPartToBlocks bps)
  return $ note blksList
runToInlines (Endnote bps) = do
  blksList <- smushBlocks <$> (mapM bodyPartToBlocks bps)
  return $ note blksList
runToInlines (InlineDrawing fp bs ext) = do
  mediaBag <- gets docxMediaBag
  modify $ \s -> s { docxMediaBag = insertMedia fp Nothing bs mediaBag }
  return $ imageWith (extentToAttr ext) fp "" ""

extentToAttr :: Extent -> Attr
extentToAttr (Just (w, h)) =
  ("", [], [("width", showDim w), ("height", showDim h)] )
  where
    showDim d = show (d / 914400) ++ "in"
extentToAttr _ = nullAttr

parPartToInlines :: ParPart -> DocxContext Inlines
parPartToInlines (PlainRun r) = runToInlines r
parPartToInlines (Insertion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> smushInlines <$> mapM runToInlines runs
    RejectChanges -> return mempty
    AllChanges    -> do
      ils <- smushInlines <$> mapM runToInlines runs
      let attr = ("", ["insertion"], [("author", author), ("date", date)])
      return $ spanWith attr ils
parPartToInlines (Deletion _ author date runs) = do
  opts <- asks docxOptions
  case readerTrackChanges opts of
    AcceptChanges -> return mempty
    RejectChanges -> smushInlines <$> mapM runToInlines runs
    AllChanges    -> do
      ils <- smushInlines <$> mapM runToInlines runs
      let attr = ("", ["deletion"], [("author", author), ("date", date)])
      return $ spanWith attr ils
parPartToInlines (BookMark _ anchor) | anchor `elem` dummyAnchors =
  return mempty
parPartToInlines (BookMark _ anchor) =
  -- We record these, so we can make sure not to overwrite
  -- user-defined anchor links with header auto ids.
  do
    -- get whether we're in a header.
    inHdrBool <- asks docxInHeaderBlock
    -- Get the anchor map.
    anchorMap <- gets docxAnchorMap
    -- We don't want to rewrite if we're in a header, since we'll take
    -- care of that later, when we make the header anchor. If the
    -- bookmark were already in uniqueIdent form, this would lead to a
    -- duplication. Otherwise, we check to see if the id is already in
    -- there. Rewrite if necessary. This will have the possible effect
    -- of rewriting user-defined anchor links. However, since these
    -- are not defined in pandoc, it seems like a necessary evil to
    -- avoid an extra pass.
    let newAnchor =
          if not inHdrBool && anchor `elem` (M.elems anchorMap)
          then uniqueIdent [Str anchor] (Set.fromList $ M.elems anchorMap)
          else anchor
    unless inHdrBool
      (modify $ \s -> s { docxAnchorMap = M.insert anchor newAnchor anchorMap})
    return $ spanWith (newAnchor, ["anchor"], []) mempty
parPartToInlines (Drawing fp bs ext) = do
  mediaBag <- gets docxMediaBag
  modify $ \s -> s { docxMediaBag = insertMedia fp Nothing bs mediaBag }
  return $ imageWith (extentToAttr ext) fp "" ""
parPartToInlines (InternalHyperLink anchor runs) = do
  ils <- smushInlines <$> mapM runToInlines runs
  return $ link ('#' : anchor) "" ils
parPartToInlines (ExternalHyperLink target runs) = do
  ils <- smushInlines <$> mapM runToInlines runs
  return $ link target "" ils
parPartToInlines (PlainOMath exps) = do
  return $ math $ writeTeX exps

isAnchorSpan :: Inline -> Bool
isAnchorSpan (Span (_, classes, kvs) ils) =
  classes == ["anchor"] &&
  null kvs &&
  null ils
isAnchorSpan _ = False

dummyAnchors :: [String]
dummyAnchors = ["_GoBack"]

makeHeaderAnchor :: Blocks -> DocxContext Blocks
makeHeaderAnchor bs = case viewl $ unMany bs of
  (x :< xs) -> do
    x' <- (makeHeaderAnchor' x)
    xs' <- (makeHeaderAnchor $ Many xs)
    return $ (singleton x') <> xs'
  EmptyL    -> return mempty

makeHeaderAnchor' :: Block -> DocxContext Block
-- If there is an anchor already there (an anchor span in the header,
-- to be exact), we rename and associate the new id with the old one.
makeHeaderAnchor' (Header n (_, classes, kvs) ils)
  | (c:cs) <- filter isAnchorSpan ils
  , (Span (ident, ["anchor"], _) _) <- c = do
    hdrIDMap <- gets docxAnchorMap
    let newIdent = uniqueIdent ils (Set.fromList $ M.elems hdrIDMap)
    modify $ \s -> s {docxAnchorMap = M.insert ident newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) (ils \\ (c:cs))
-- Otherwise we just give it a name, and register that name (associate
-- it with itself.)
makeHeaderAnchor' (Header n (_, classes, kvs) ils) =
  do
    hdrIDMap <- gets docxAnchorMap
    let newIdent = uniqueIdent ils (Set.fromList $ M.elems hdrIDMap)
    modify $ \s -> s {docxAnchorMap = M.insert newIdent newIdent hdrIDMap}
    return $ Header n (newIdent, classes, kvs) ils
makeHeaderAnchor' blk = return blk

-- Rewrite a standalone paragraph block as a plain
singleParaToPlain :: Blocks -> Blocks
singleParaToPlain blks
  | (Para (ils) :< seeq) <- viewl $ unMany blks
  , Seq.null seeq =
      singleton $ Plain ils
singleParaToPlain blks = blks

cellToBlocks :: Cell -> DocxContext Blocks
cellToBlocks (Cell bps) = do
  blks <- smushBlocks <$> mapM bodyPartToBlocks bps
  return $ fromList $ blocksToDefinitions $ blocksToBullets $ toList blks

rowToBlocksList :: Row -> DocxContext [Blocks]
rowToBlocksList (Row cells) = do
  blksList <- mapM cellToBlocks cells
  return $ map singleParaToPlain blksList

trimLineBreaks :: [Inline] -> [Inline]
trimLineBreaks [] = []
trimLineBreaks (LineBreak : ils) = trimLineBreaks ils
trimLineBreaks ils
  | (LineBreak : ils') <- reverse ils = trimLineBreaks (reverse ils')
trimLineBreaks ils = ils

parStyleToTransform :: ParagraphStyle -> (Blocks -> Blocks)
parStyleToTransform pPr
  | (c:cs) <- pStyle pPr
  , c `elem` divsToKeep =
    let pPr' = pPr { pStyle = cs }
    in
     (divWith ("", [c], [])) . (parStyleToTransform pPr')
  | (c:cs) <- pStyle pPr,
    c `elem` listParagraphDivs =
      let pPr' = pPr { pStyle = cs, indentation = Nothing}
      in
       (divWith ("", [c], [])) . (parStyleToTransform pPr')
  | (_:cs) <- pStyle pPr
  , Just True <- pBlockQuote pPr =
    let pPr' = pPr { pStyle = cs }
    in
     blockQuote . (parStyleToTransform pPr')
  | (_:cs) <- pStyle pPr =
      let pPr' = pPr { pStyle = cs}
      in
       parStyleToTransform pPr'
  | null (pStyle pPr)
  , Just left <- indentation pPr >>= leftParIndent
  , Just hang <- indentation pPr >>= hangingParIndent =
    let pPr' = pPr { indentation = Nothing }
    in
     case (left - hang) > 0 of
       True -> blockQuote . (parStyleToTransform pPr')
       False -> parStyleToTransform pPr'
  | null (pStyle pPr),
    Just left <- indentation pPr >>= leftParIndent =
      let pPr' = pPr { indentation = Nothing }
      in
       case left > 0 of
         True -> blockQuote . (parStyleToTransform pPr')
         False -> parStyleToTransform pPr'
parStyleToTransform _ = id

bodyPartToBlocks :: BodyPart -> DocxContext Blocks
bodyPartToBlocks (Paragraph pPr parparts)
  | not $ null $ codeDivs `intersect` (pStyle pPr) =
    return
    $ parStyleToTransform pPr
    $ codeBlock
    $ concatMap parPartToString parparts
  | Just (style, n) <- pHeading pPr = do
    ils <- local (\s-> s{docxInHeaderBlock=True}) $
           (smushInlines <$> mapM parPartToInlines parparts)
    makeHeaderAnchor $
      headerWith ("", delete style (pStyle pPr), []) n ils
  | otherwise = do
    ils <- smushInlines <$> mapM parPartToInlines parparts >>=
           (return . fromList . trimLineBreaks . normalizeSpaces . toList)
    dropIls <- gets docxDropCap
    let ils' = dropIls <> ils
    if dropCap pPr
      then do modify $ \s -> s { docxDropCap = ils' }
              return mempty
      else do modify $ \s -> s { docxDropCap = mempty }
              return $ case isNull ils' of
                True -> mempty
                _ -> parStyleToTransform pPr $ para ils'
bodyPartToBlocks (ListItem pPr numId lvl (Just levelInfo) parparts) = do
  let
    kvs = case levelInfo of
      (_, fmt, txt, Just start) -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   , ("start", (show start))
                                   ]

      (_, fmt, txt, Nothing)    -> [ ("level", lvl)
                                   , ("num-id", numId)
                                   , ("format", fmt)
                                   , ("text", txt)
                                   ]
  blks <- bodyPartToBlocks (Paragraph pPr parparts)
  return $ divWith ("", ["list-item"], kvs) blks
bodyPartToBlocks (ListItem pPr _ _ _ parparts) = 
  let pPr' = pPr {pStyle = "ListParagraph": (pStyle pPr)}
  in
    bodyPartToBlocks $ Paragraph pPr' parparts
bodyPartToBlocks (Tbl _ _ _ []) =
  return $ para mempty
bodyPartToBlocks (Tbl cap _ look (r:rs)) = do
  let caption = text cap
      (hdr, rows) = case firstRowFormatting look of
        True -> (Just r, rs)
        False -> (Nothing, r:rs)
  hdrCells <- case hdr of
    Just r' -> rowToBlocksList r'
    Nothing -> return []

  cells <- mapM rowToBlocksList rows

  let size = case null hdrCells of
        True -> length $ head cells
        False -> length $ hdrCells
      --
      -- The two following variables (horizontal column alignment and
      -- relative column widths) go to the default at the
      -- moment. Width information is in the TblGrid field of the Tbl,
      -- so should be possible. Alignment might be more difficult,
      -- since there doesn't seem to be a column entity in docx.
      alignments = replicate size AlignDefault
      widths = replicate size 0 :: [Double]

  return $ table caption (zip alignments widths) hdrCells cells
bodyPartToBlocks (OMathPara e) = do
  return $ para $ displayMath (writeTeX e)


-- replace targets with generated anchors.
rewriteLink' :: Inline -> DocxContext Inline
rewriteLink' l@(Link attr ils ('#':target, title)) = do
  anchorMap <- gets docxAnchorMap
  return $ case M.lookup target anchorMap of
    Just newTarget -> (Link attr ils ('#':newTarget, title))
    Nothing        -> l
rewriteLink' il = return il

rewriteLinks :: [Block] -> DocxContext [Block]
rewriteLinks = mapM (walkM rewriteLink')

bodyToOutput :: Body -> DocxContext (Meta, [Block], MediaBag)
bodyToOutput (Body bps) = do
  let (metabps, blkbps) = sepBodyParts bps
  meta <- bodyPartsToMeta metabps
  blks <- smushBlocks <$> mapM bodyPartToBlocks blkbps
  blks' <- rewriteLinks $ blocksToDefinitions $ blocksToBullets $ toList blks
  mediaBag <- gets docxMediaBag
  return $ (meta,
            blks',
            mediaBag)

docxToOutput :: ReaderOptions -> Docx -> Either PandocError (Meta, [Block], MediaBag)
docxToOutput opts (Docx (Document _ body)) =
  let dEnv   = def { docxOptions  = opts} in
   evalDocxContext (bodyToOutput body) dEnv def
