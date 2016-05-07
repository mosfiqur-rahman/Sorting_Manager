{-# LANGUAGE CPP #-}

module Text.Pandoc.Writers.OPML ( writeOPML) where
import Text.Pandoc.Definition
import Text.Pandoc.XML
import Text.Pandoc.Writers.Shared
import Text.Pandoc.Shared
import Text.Pandoc.Options
import Text.Pandoc.Templates (renderTemplate')
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Writers.Markdown (writeMarkdown)
import Text.Pandoc.Pretty
import Text.Pandoc.Compat.Time
import qualified Text.Pandoc.Builder as B

-- | Convert Pandoc document to string in OPML format.
writeOPML :: WriterOptions -> Pandoc -> String
writeOPML opts (Pandoc meta blocks) =
  let elements = hierarchicalize blocks
      colwidth = if writerWrapText opts == WrapAuto
                    then Just $ writerColumns opts
                    else Nothing
      meta' = B.setMeta "date" (B.str $ convertDate $ docDate meta) meta
      Just metadata = metaToJSON opts
                      (Just . writeMarkdown def . Pandoc nullMeta)
                      (Just . trimr . writeMarkdown def . Pandoc nullMeta .
                         (\ils -> [Plain ils]))
                      meta'
      main     = render colwidth $ vcat (map (elementToOPML opts) elements)
      context = defField "body" main metadata
  in  if writerStandalone opts
         then renderTemplate' (writerTemplate opts) context
         else main

writeHtmlInlines :: [Inline] -> String
writeHtmlInlines ils = trim $ writeHtmlString def
                            $ Pandoc nullMeta [Plain ils]

-- date format: RFC 822: Thu, 14 Jul 2005 23:41:05 GMT
showDateTimeRFC822 :: UTCTime -> String
showDateTimeRFC822 = formatTime defaultTimeLocale "%a, %d %b %Y %X %Z"

convertDate :: [Inline] -> String
convertDate ils = maybe "" showDateTimeRFC822 $
#if MIN_VERSION_time(1,5,0)
  parseTimeM True
#else
  parseTime
#endif
  defaultTimeLocale "%F" =<< (normalizeDate $ stringify ils)

-- | Convert an Element to OPML.
elementToOPML :: WriterOptions -> Element -> Doc
elementToOPML _ (Blk _) = empty
elementToOPML opts (Sec _ _num _ title elements) =
  let isBlk (Blk _) = True
      isBlk _     = False
      fromBlk (Blk x) = x
      fromBlk _ = error "fromBlk called on non-block"
      (blocks, rest) = span isBlk elements
      attrs = [("text", writeHtmlInlines title)] ++
              [("_note", writeMarkdown def (Pandoc nullMeta
                              (map fromBlk blocks)))
                | not (null blocks)]
  in  inTags True "outline" attrs $
      vcat (map (elementToOPML opts) rest)
