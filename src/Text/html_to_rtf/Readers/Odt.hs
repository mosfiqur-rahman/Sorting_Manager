{-# LANGUAGE PatternGuards #-}



module Text.Pandoc.Readers.Odt ( readOdt ) where

import           Codec.Archive.Zip
import qualified Text.XML.Light                        as XML

import qualified Data.ByteString.Lazy                  as B

import           Text.Pandoc.Definition
import           Text.Pandoc.Error
import           Text.Pandoc.Options
import           Text.Pandoc.MediaBag
import qualified Text.Pandoc.UTF8                      as UTF8

import           Text.Pandoc.Readers.Odt.ContentReader
import           Text.Pandoc.Readers.Odt.StyleReader

import           Text.Pandoc.Readers.Odt.Generic.XMLConverter
import           Text.Pandoc.Readers.Odt.Generic.Fallible

--
readOdt :: ReaderOptions
        -> B.ByteString
        -> Either PandocError (Pandoc, MediaBag)
readOdt _ bytes = case bytesToOdt bytes of
                    Right pandoc -> Right (pandoc , mempty)
                    Left  err    -> Left err

--
bytesToOdt :: B.ByteString -> Either PandocError Pandoc
bytesToOdt bytes = archiveToOdt $ toArchive bytes

--
archiveToOdt :: Archive -> Either PandocError Pandoc
archiveToOdt archive
  | Just contentEntry <- findEntryByPath "content.xml" archive
  , Just stylesEntry  <- findEntryByPath "styles.xml"  archive
  , Just contentElem  <- entryToXmlElem contentEntry
  , Just stylesElem   <- entryToXmlElem stylesEntry
  , Right styles      <- chooseMax (readStylesAt stylesElem )
                                   (readStylesAt contentElem)
  , startState        <- readerState styles
  , Right pandoc      <- runConverter' read_body
                                       startState
                                       contentElem
  = Right pandoc

  | otherwise
    -- Not very detailed, but I don't think more information would be helpful
  = Left $ ParseFailure "Couldn't parse odt file."

--
entryToXmlElem :: Entry -> Maybe XML.Element
entryToXmlElem = XML.parseXMLDoc . UTF8.toStringLazy . fromEntry
