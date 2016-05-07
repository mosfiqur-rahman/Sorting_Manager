module Text.Pandoc.Readers.DocBook ( readDocBook ) where
import Data.Char (toUpper)
import Text.Pandoc.Shared (safeRead)
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Builder
import Text.XML.Light
import Text.Pandoc.Compat.TagSoupEntity (lookupEntity)
import Data.Either (rights)
import Data.Generics
import Data.Char (isSpace)
import Control.Monad.State
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Text.TeXMath (readMathML, writeTeX)
import Text.Pandoc.Error (PandocError)
import Text.Pandoc.Compat.Except
import Data.Default
import Data.Foldable (asum)



type DB = ExceptT PandocError (State DBState)

data DBState = DBState{ dbSectionLevel :: Int
                      , dbQuoteType    :: QuoteType
                      , dbMeta         :: Meta
                      , dbAcceptsMeta  :: Bool
                      , dbBook         :: Bool
                      , dbFigureTitle  :: Inlines
                      , dbContent      :: [Content]
                      } deriving Show

instance Default DBState where
  def = DBState{ dbSectionLevel = 0
               , dbQuoteType = DoubleQuote
               , dbMeta = mempty
               , dbAcceptsMeta = False
               , dbBook = False
               , dbFigureTitle = mempty
               , dbContent = [] }


readDocBook :: ReaderOptions -> String -> Either PandocError Pandoc
readDocBook _ inp  = (\blocks -> Pandoc (dbMeta st') (toList . mconcat $ blocks)) <$>  bs
  where (bs , st') = flip runState (def{ dbContent = tree }) . runExceptT . mapM parseBlock $ tree
        tree = normalizeTree . parseXML . handleInstructions $ inp

-- We treat <?asciidoc-br?> specially (issue #1236), converting it
-- to <br/>, since xml-light doesn't parse the instruction correctly.
-- Other xml instructions are simply removed from the input stream.
handleInstructions :: String -> String
handleInstructions ('<':'?':'a':'s':'c':'i':'i':'d':'o':'c':'-':'b':'r':'?':'>':xs) = '<':'b':'r':'/':'>': handleInstructions xs
handleInstructions xs = case break (=='<') xs of
                             (ys, [])     -> ys
                             ([], '<':zs) -> '<' : handleInstructions zs
                             (ys, zs) -> ys ++ handleInstructions zs

getFigure :: Element -> DB Blocks
getFigure e = do
  tit <- case filterChild (named "title") e of
              Just t -> getInlines t
              Nothing -> return mempty
  modify $ \st -> st{ dbFigureTitle = tit }
  res <- getBlocks e
  modify $ \st -> st{ dbFigureTitle = mempty }
  return res

-- normalize input, consolidating adjacent Text and CRef elements
normalizeTree :: [Content] -> [Content]
normalizeTree = everywhere (mkT go)
  where go :: [Content] -> [Content]
        go (Text (CData CDataRaw _ _):xs) = xs
        go (Text (CData CDataText s1 z):Text (CData CDataText s2 _):xs) =
           Text (CData CDataText (s1 ++ s2) z):xs
        go (Text (CData CDataText s1 z):CRef r:xs) =
           Text (CData CDataText (s1 ++ convertEntity r) z):xs
        go (CRef r:Text (CData CDataText s1 z):xs) =
             Text (CData CDataText (convertEntity r ++ s1) z):xs
        go (CRef r1:CRef r2:xs) =
             Text (CData CDataText (convertEntity r1 ++ convertEntity r2) Nothing):xs
        go xs = xs

convertEntity :: String -> String
convertEntity e = maybe (map toUpper e) (:[]) (lookupEntity e)

-- convenience function to get an attribute value, defaulting to ""
attrValue :: String -> Element -> String
attrValue attr elt =
  case lookupAttrBy (\x -> qName x == attr) (elAttribs elt) of
    Just z  -> z
    Nothing -> ""

-- convenience function
named :: String -> Element -> Bool
named s e = qName (elName e) == s

--

acceptingMetadata :: DB a -> DB a
acceptingMetadata p = do
  modify (\s -> s { dbAcceptsMeta = True } )
  res <- p
  modify (\s -> s { dbAcceptsMeta = False })
  return res

checkInMeta :: Monoid a => DB () -> DB a
checkInMeta p = do
  accepts <- dbAcceptsMeta <$> get
  when accepts p
  return mempty



addMeta :: ToMetaValue a => String -> a -> DB ()
addMeta field val = modify (setMeta field val)

instance HasMeta DBState where
  setMeta field v s =  s {dbMeta = setMeta field v (dbMeta s)}
  deleteMeta field s = s {dbMeta = deleteMeta field (dbMeta s)}

isBlockElement :: Content -> Bool
isBlockElement (Elem e) = qName (elName e) `elem` blocktags
  where blocktags = ["toc","index","para","formalpara","simpara",
           "ackno","epigraph","blockquote","bibliography","bibliodiv",
           "biblioentry","glossee","glosseealso","glossary",
           "glossdiv","glosslist","chapter","appendix","preface",
           "bridgehead","sect1","sect2","sect3","sect4","sect5","section",
           "refsect1","refsect2","refsect3","refsection",
           "important","caution","note","tip","warning","qandadiv",
           "question","answer","abstract","itemizedlist","orderedlist",
           "variablelist","article","book","table","informaltable",
           "informalexample",
           "screen","programlisting","example","calloutlist"]
isBlockElement _ = False

-- Trim leading and trailing newline characters
trimNl :: String -> String
trimNl = reverse . go . reverse . go
  where go ('\n':xs) = xs
        go xs        = xs

-- meld text into beginning of first paragraph of Blocks.
-- assumes Blocks start with a Para; if not, does nothing.
addToStart :: Inlines -> Blocks -> Blocks
addToStart toadd bs =
  case toList bs of
    (Para xs : rest) -> para (toadd <> fromList xs) <> fromList rest
    _                -> bs

-- function that is used by both mediaobject (in parseBlock)
-- and inlinemediaobject (in parseInline)
-- A DocBook mediaobject is a wrapper around a set of alternative presentations
getMediaobject :: Element -> DB Inlines
getMediaobject e = do
  (imageUrl, attr) <-
    case filterChild (named "imageobject") e of
      Nothing  -> return (mempty, nullAttr)
      Just z   -> case filterChild (named "imagedata") z of
                    Nothing -> return (mempty, nullAttr)
                    Just i  -> let atVal a = attrValue a i
                                   w = case atVal "width" of
                                         "" -> []
                                         d  -> [("width", d)]
                                   h = case atVal "depth" of
                                         "" -> []
                                         d  -> [("height", d)]
                                   atr = (atVal "id", words $ atVal "role", w ++ h)
                               in  return (atVal "fileref", atr)
  let getCaption el = case filterChild (\x -> named "caption" x
                                            || named "textobject" x
                                            || named "alt" x) el of
                        Nothing -> return mempty
                        Just z  -> mconcat <$> (mapM parseInline $ elContent z)
  figTitle <- gets dbFigureTitle
  let (caption, title) = if isNull figTitle
                            then (getCaption e, "")
                            else (return figTitle, "fig:")
  liftM (imageWith attr imageUrl title) caption

getBlocks :: Element -> DB Blocks
getBlocks e =  mconcat <$> (mapM parseBlock $ elContent e)


parseBlock :: Content -> DB Blocks
parseBlock (Text (CData CDataRaw _ _)) = return mempty -- DOCTYPE
parseBlock (Text (CData _ s _)) = if all isSpace s
                                     then return mempty
                                     else return $ plain $ trimInlines $ text s
parseBlock (CRef x) = return $ plain $ str $ map toUpper x
parseBlock (Elem e) =
  case qName (elName e) of
        "toc"   -> return mempty -- skip TOC, since in pandoc it's autogenerated
        "index" -> return mempty -- skip index, since page numbers meaningless
        "para"  -> parseMixed para (elContent e)
        "formalpara" -> do
           tit <- case filterChild (named "title") e of
                        Just t  -> (para . strong . (<> str ".")) <$>
                                     getInlines t
                        Nothing -> return mempty
           (tit <>) <$> parseMixed para (elContent e)
        "simpara"  -> parseMixed para (elContent e)
        "ackno"  -> parseMixed para (elContent e)
        "epigraph" -> parseBlockquote
        "blockquote" -> parseBlockquote
        "attribution" -> return mempty
        "titleabbrev" -> return mempty
        "authorinitials" -> return mempty
        "title" ->  checkInMeta getTitle
        "author" -> checkInMeta getAuthor
        "authorgroup" -> checkInMeta getAuthorGroup
        "releaseinfo" -> checkInMeta (getInlines e >>= addMeta "release")
        "date" -> checkInMeta getDate
        "bibliography" -> sect 0
        "bibliodiv" -> sect 1
        "biblioentry" -> parseMixed para (elContent e)
        "bibliomixed" -> parseMixed para (elContent e)
        "glosssee" -> para . (\ils -> text "See " <> ils <> str ".")
                         <$> getInlines e
        "glossseealso" -> para . (\ils -> text "See also " <> ils <> str ".")
                         <$> getInlines e
        "glossary" -> sect 0
        "glossdiv" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "glosslist" -> definitionList <$>
                  mapM parseGlossEntry (filterChildren (named "glossentry") e)
        "chapter" -> sect 0
        "appendix" -> sect 0
        "preface" -> sect 0
        "bridgehead" -> para . strong <$> getInlines e
        "sect1" -> sect 1
        "sect2" -> sect 2
        "sect3" -> sect 3
        "sect4" -> sect 4
        "sect5" -> sect 5
        "section" -> gets dbSectionLevel >>= sect . (+1)
        "refsect1" -> sect 1
        "refsect2" -> sect 2
        "refsect3" -> sect 3
        "refsection" -> gets dbSectionLevel >>= sect . (+1)
        "important" -> blockQuote . (para (strong $ str "Important") <>)
                        <$> getBlocks e
        "caution" -> blockQuote . (para (strong $ str "Caution") <>)
                        <$> getBlocks e
        "note" -> blockQuote . (para (strong $ str "Note") <>)
                        <$> getBlocks e
        "tip" -> blockQuote . (para (strong $ str "Tip") <>)
                        <$> getBlocks e
        "warning" -> blockQuote . (para (strong $ str "Warning") <>)
                        <$> getBlocks e
        "area" -> return mempty
        "areaset" -> return mempty
        "areaspec" -> return mempty
        "qandadiv" -> gets dbSectionLevel >>= sect . (+1)
        "question" -> addToStart (strong (str "Q:") <> str " ") <$> getBlocks e
        "answer" -> addToStart (strong (str "A:") <> str " ") <$> getBlocks e
        "abstract" -> blockQuote <$> getBlocks e
        "calloutlist" -> bulletList <$> callouts
        "itemizedlist" -> bulletList <$> listitems
        "orderedlist" -> do
          let listStyle = case attrValue "numeration" e of
                               "arabic"     -> Decimal
                               "loweralpha" -> LowerAlpha
                               "upperalpha" -> UpperAlpha
                               "lowerroman" -> LowerRoman
                               "upperroman" -> UpperRoman
                               _            -> Decimal
          let start = fromMaybe 1 $
                      (attrValue "override" <$> filterElement (named "listitem") e)
                       >>= safeRead
          orderedListWith (start,listStyle,DefaultDelim)
            <$> listitems
        "variablelist" -> definitionList <$> deflistitems
        "figure" -> getFigure e
        "mediaobject" -> para <$> getMediaobject e
        "caption" -> return mempty
        "info" -> metaBlock
        "articleinfo" -> metaBlock
        "sectioninfo" -> return mempty  -- keywords & other metadata
        "refsectioninfo" -> return mempty  -- keywords & other metadata
        "refsect1info" -> return mempty  -- keywords & other metadata
        "refsect2info" -> return mempty  -- keywords & other metadata
        "refsect3info" -> return mempty  -- keywords & other metadata
        "sect1info" -> return mempty  -- keywords & other metadata
        "sect2info" -> return mempty  -- keywords & other metadata
        "sect3info" -> return mempty  -- keywords & other metadata
        "sect4info" -> return mempty  -- keywords & other metadata
        "sect5info" -> return mempty  -- keywords & other metadata
        "chapterinfo" -> return mempty  -- keywords & other metadata
        "glossaryinfo" -> return mempty  -- keywords & other metadata
        "appendixinfo" -> return mempty  -- keywords & other metadata
        "bookinfo" -> metaBlock
        "article" -> modify (\st -> st{ dbBook = False }) >>
                           getBlocks e
        "book" -> modify (\st -> st{ dbBook = True }) >>  getBlocks e
        "table" -> parseTable
        "informaltable" -> parseTable
        "informalexample" -> divWith ("", ["informalexample"], []) <$>
                             getBlocks e
        "literallayout" -> codeBlockWithLang
        "screen" -> codeBlockWithLang
        "programlisting" -> codeBlockWithLang
        "?xml"  -> return mempty
        _       -> getBlocks e
   where parseMixed container conts = do
           let (ils,rest) = break isBlockElement conts
           ils' <- (trimInlines . mconcat) <$> mapM parseInline ils
           let p = if ils' == mempty then mempty else container ils'
           case rest of
                 []     -> return p
                 (r:rs) -> do
                    b <- parseBlock r
                    x <- parseMixed container rs
                    return $ p <> b <> x
         codeBlockWithLang = do
           let classes' = case attrValue "language" e of
                                ""   -> []
                                x    -> [x]
           return $ codeBlockWith (attrValue "id" e, classes', [])
                  $ trimNl $ strContentRecursive e
         parseBlockquote = do
            attrib <- case filterChild (named "attribution") e of
                             Nothing  -> return mempty
                             Just z   -> (para . (str "— " <>) . mconcat)
                                         <$> (mapM parseInline $ elContent z)
            contents <- getBlocks e
            return $ blockQuote (contents <> attrib)
         listitems = mapM getBlocks $ filterChildren (named "listitem") e
         callouts = mapM getBlocks $ filterChildren (named "callout") e
         deflistitems = mapM parseVarListEntry $ filterChildren
                     (named "varlistentry") e
         parseVarListEntry e' = do
                     let terms = filterChildren (named "term") e'
                     let items = filterChildren (named "listitem") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         parseGlossEntry e' = do
                     let terms = filterChildren (named "glossterm") e'
                     let items = filterChildren (named "glossdef") e'
                     terms' <- mapM getInlines terms
                     items' <- mapM getBlocks items
                     return (mconcat $ intersperse (str "; ") terms', items')
         getTitle =  do
                     tit <- getInlines e
                     subtit <-  case filterChild (named "subtitle") e of
                                  Just s  -> (text ": " <>) <$>
                                              getInlines s
                                  Nothing -> return mempty
                     addMeta "title" (tit <> subtit)

         getAuthor = (:[]) <$> getInlines e >>= addMeta "author"
         getAuthorGroup = do
          let terms = filterChildren (named "author") e
          mapM getInlines terms >>= addMeta "author"
         getDate = getInlines e >>= addMeta "date"
         parseTable = do
                      let isCaption x = named "title" x || named "caption" x
                      caption <- case filterChild isCaption e of
                                       Just t  -> getInlines t
                                       Nothing -> return mempty
                      let e' = fromMaybe e $ filterChild (named "tgroup") e
                      let isColspec x = named "colspec" x || named "col" x
                      let colspecs = case filterChild (named "colgroup") e' of
                                           Just c -> filterChildren isColspec c
                                           _      -> filterChildren isColspec e'
                      let isRow x = named "row" x || named "tr" x
                      headrows <- case filterChild (named "thead") e' of
                                       Just h  -> case filterChild isRow h of
                                                       Just x  -> parseRow x
                                                       Nothing -> return []
                                       Nothing -> return []
                      bodyrows <- case filterChild (named "tbody") e' of
                                       Just b  -> mapM parseRow
                                                  $ filterChildren isRow b
                                       Nothing -> mapM parseRow
                                                  $ filterChildren isRow e'
                      let toAlignment c = case findAttr (unqual "align") c of
                                                Just "left"   -> AlignLeft
                                                Just "right"  -> AlignRight
                                                Just "center" -> AlignCenter
                                                _             -> AlignDefault
                      let toWidth c = case findAttr (unqual "colwidth") c of
                                                Just w -> fromMaybe 0
                                                   $ safeRead $ '0': filter (\x ->
                                                     (x >= '0' && x <= '9')
                                                      || x == '.') w
                                                Nothing -> 0 :: Double
                      let numrows = case bodyrows of
                                         []   -> 0
                                         xs   -> maximum $ map length xs
                      let aligns = case colspecs of
                                     []  -> replicate numrows AlignDefault
                                     cs  -> map toAlignment cs
                      let widths = case colspecs of
                                     []  -> replicate numrows 0
                                     cs  -> let ws = map toWidth cs
                                                tot = sum ws
                                            in  if all (> 0) ws
                                                   then map (/ tot) ws
                                                   else replicate numrows 0
                      let headrows' = if null headrows
                                         then replicate numrows mempty
                                         else headrows
                      return $ table caption (zip aligns widths)
                                 headrows' bodyrows
         isEntry x  = named "entry" x || named "td" x || named "th" x
         parseRow = mapM (parseMixed plain . elContent) . filterChildren isEntry
         sect n = do isbook <- gets dbBook
                     let n' = if isbook || n == 0 then n + 1 else n
                     headerText <- case filterChild (named "title") e `mplus`
                                        (filterChild (named "info") e >>=
                                            filterChild (named "title")) of
                                      Just t -> getInlines t
                                      Nothing -> return mempty
                     modify $ \st -> st{ dbSectionLevel = n }
                     b <- getBlocks e
                     let ident = attrValue "id" e
                     modify $ \st -> st{ dbSectionLevel = n - 1 }
                     return $ headerWith (ident,[],[]) n' headerText <> b
         metaBlock = acceptingMetadata (getBlocks e) >> return mempty

getInlines :: Element -> DB Inlines
getInlines e' = (trimInlines . mconcat) <$> (mapM parseInline $ elContent e')

strContentRecursive :: Element -> String
strContentRecursive = strContent .
  (\e' -> e'{ elContent = map elementToStr $ elContent e' })

elementToStr :: Content -> Content
elementToStr (Elem e') = Text $ CData CDataText (strContentRecursive e') Nothing
elementToStr x = x

parseInline :: Content -> DB Inlines
parseInline (Text (CData _ s _)) = return $ text s
parseInline (CRef ref) =
  return $ maybe (text $ map toUpper ref) (text . (:[])) $ lookupEntity ref
parseInline (Elem e) =
  case qName (elName e) of
        "equation" -> equation displayMath
        "informalequation" -> equation displayMath
        "inlineequation" -> equation math
        "subscript" -> subscript <$> innerInlines
        "superscript" -> superscript <$> innerInlines
        "inlinemediaobject" -> getMediaobject e
        "quote" -> do
            qt <- gets dbQuoteType
            let qt' = if qt == SingleQuote then DoubleQuote else SingleQuote
            modify $ \st -> st{ dbQuoteType = qt' }
            contents <- innerInlines
            modify $ \st -> st{ dbQuoteType = qt }
            return $ if qt == SingleQuote
                        then singleQuoted contents
                        else doubleQuoted contents
        "simplelist" -> simpleList
        "segmentedlist" -> segmentedList
        "classname" -> codeWithLang
        "code" -> codeWithLang
        "filename" -> codeWithLang
        "literal" -> codeWithLang
        "computeroutput" -> codeWithLang
        "prompt" -> codeWithLang
        "parameter" -> codeWithLang
        "option" -> codeWithLang
        "optional" -> do x <- getInlines e
                         return $ str "[" <> x <> str "]"
        "markup" -> codeWithLang
        "wordasword" -> emph <$> innerInlines
        "command" -> codeWithLang
        "varname" -> codeWithLang
        "function" -> codeWithLang
        "type"    -> codeWithLang
        "symbol"  -> codeWithLang
        "constant" -> codeWithLang
        "userinput" -> codeWithLang
        "varargs" -> return $ code "(...)"
        "keycap" -> return (str $ strContent e)
        "keycombo" -> keycombo <$> (mapM parseInline $ elContent e)
        "menuchoice" -> menuchoice <$> (mapM parseInline $
                                        filter isGuiMenu $ elContent e)
        "xref" -> do
            content <- dbContent <$> get
            let linkend = attrValue "linkend" e
            let title = case attrValue "endterm" e of
                            ""      -> maybe "???" xrefTitleByElem (findElementById linkend content)
                            endterm -> maybe "???" strContent (findElementById endterm content)
            return $ link ('#' : linkend) "" (singleton (Str title))
        "email" -> return $ link ("mailto:" ++ strContent e) ""
                          $ str $ strContent e
        "uri" -> return $ link (strContent e) "" $ str $ strContent e
        "ulink" -> link (attrValue "url" e) "" <$> innerInlines
        "link" -> do
             ils <- innerInlines
             let href = case findAttr (QName "href" (Just "http://www.w3.org/1999/xlink") Nothing) e of
                               Just h -> h
                               _      -> ('#' : attrValue "linkend" e)
             let ils' = if ils == mempty then str href else ils
             let attr = (attrValue "id" e, words $ attrValue "role" e, [])
             return $ linkWith attr href "" ils'
        "foreignphrase" -> emph <$> innerInlines
        "emphasis" -> case attrValue "role" e of
                             "bold"   -> strong <$> innerInlines
                             "strong" -> strong <$> innerInlines
                             "strikethrough" -> strikeout <$> innerInlines
                             _        -> emph <$> innerInlines
        "footnote" -> (note . mconcat) <$> (mapM parseBlock $ elContent e)
        "title" -> return mempty
        "affiliation" -> return mempty
        -- Note: this isn't a real docbook tag; it's what we convert
        -- <?asciidor-br?> to in handleInstructions, above.  A kludge to
        -- work around xml-light's inability to parse an instruction.
        "br" -> return linebreak
        _          -> innerInlines
   where innerInlines = (trimInlines . mconcat) <$>
                          (mapM parseInline $ elContent e)
         equation constructor = return $ mconcat $
           map (constructor . writeTeX)
           $ rights
           $ map (readMathML . showElement . everywhere (mkT removePrefix))
           $ filterChildren (\x -> qName (elName x) == "math" &&
                                   qPrefix (elName x) == Just "mml") e
         removePrefix elname = elname { qPrefix = Nothing }
         codeWithLang = do
           let classes' = case attrValue "language" e of
                               "" -> []
                               l  -> [l]
           return $ codeWith (attrValue "id" e,classes',[]) $ strContentRecursive e
         simpleList = (mconcat . intersperse (str "," <> space)) <$> mapM getInlines
                         (filterChildren (named "member") e)
         segmentedList = do
           tit <- maybe (return mempty) getInlines $ filterChild (named "title") e
           segtits <- mapM getInlines $ filterChildren (named "segtitle") e
           segitems <- mapM (mapM getInlines . filterChildren (named "seg"))
                          $ filterChildren (named "seglistitem") e
           let toSeg = mconcat . zipWith (\x y -> strong (x <> str ":") <> space <>
                                  y <> linebreak) segtits
           let segs = mconcat $ map toSeg segitems
           let tit' = if tit == mempty
                         then mempty
                         else strong tit <> linebreak
           return $ linebreak <> tit' <> segs
         keycombo = spanWith ("",["keycombo"],[]) .
                    mconcat . intersperse (str "+")
         menuchoice = spanWith ("",["menuchoice"],[]) .
                    mconcat . intersperse (text " > ")
         isGuiMenu (Elem x) = named "guimenu" x || named "guisubmenu" x ||
                              named "guimenuitem" x
         isGuiMenu _        = False

         findElementById idString content
            = asum [filterElement (\x -> attrValue "id" x == idString) el | Elem el <- content]

         -- Use the 'xreflabel' attribute for getting the title of a xref link;
         -- if there's no such attribute, employ some heuristics based on what
         -- docbook-xsl does.
         xrefTitleByElem el
             | not (null xrefLabel) = xrefLabel
             | otherwise            = case qName (elName el) of
                  "chapter"      -> descendantContent "title" el
                  "sect1"        -> descendantContent "title" el
                  "sect2"        -> descendantContent "title" el
                  "sect3"        -> descendantContent "title" el
                  "sect4"        -> descendantContent "title" el
                  "sect5"        -> descendantContent "title" el
                  "cmdsynopsis"  -> descendantContent "command" el
                  "funcsynopsis" -> descendantContent "function" el
                  _              -> qName (elName el) ++ "_title"
          where
            xrefLabel = attrValue "xreflabel" el
            descendantContent name = maybe "???" strContent
                                   . findElement (QName name Nothing Nothing)
