
module Text.Pandoc.XML ( escapeCharForXML,
                         escapeStringForXML,
                         inTags,
                         selfClosingTag,
                         inTagsSimple,
                         inTagsIndented,
                         toEntities,
                         fromEntities ) where

import Text.Pandoc.Pretty
import Data.Char (ord, isAscii, isSpace)
import Text.Pandoc.Compat.TagSoupEntity (lookupEntity)

-- | Escape one character as needed for XML.
escapeCharForXML :: Char -> String
escapeCharForXML x = case x of
                       '&'    -> "&amp;"
                       '<'    -> "&lt;"
                       '>'    -> "&gt;"
                       '"'    -> "&quot;"
                       c      -> [c]

-- | Escape string as needed for XML.  Entity references are not preserved.
escapeStringForXML :: String -> String
escapeStringForXML = concatMap escapeCharForXML

-- | Escape newline characters as &#10;
escapeNls :: String -> String
escapeNls (x:xs)
  | x == '\n' = "&#10;" ++ escapeNls xs
  | otherwise = x : escapeNls xs
escapeNls []     = []

-- | Return a text object with a string of formatted XML attributes.
attributeList :: [(String, String)] -> Doc
attributeList = hcat . map
  (\(a, b) -> text (' ' : escapeStringForXML a ++ "=\"" ++
  escapeNls (escapeStringForXML b) ++ "\""))

-- | Put the supplied contents between start and end tags of tagType,
--   with specified attributes and (if specified) indentation.
inTags:: Bool -> String -> [(String, String)] -> Doc -> Doc
inTags isIndented tagType attribs contents =
  let openTag = char '<' <> text tagType <> attributeList attribs <>
                char '>'
      closeTag  = text "</" <> text tagType <> char '>'
  in  if isIndented
         then openTag $$ nest 2 contents $$ closeTag
         else openTag <> contents <> closeTag

-- | Return a self-closing tag of tagType with specified attributes
selfClosingTag :: String -> [(String, String)] -> Doc
selfClosingTag tagType attribs =
  char '<' <> text tagType <> attributeList attribs <> text " />"

-- | Put the supplied contents between start and end tags of tagType.
inTagsSimple :: String -> Doc -> Doc
inTagsSimple tagType = inTags False tagType []

-- | Put the supplied contents in indented block btw start and end tags.
inTagsIndented :: String -> Doc -> Doc
inTagsIndented tagType = inTags True tagType []

-- | Escape all non-ascii characters using numerical entities.
toEntities :: String -> String
toEntities [] = ""
toEntities (c:cs)
  | isAscii c = c : toEntities cs
  | otherwise = "&#" ++ show (ord c) ++ ";" ++ toEntities cs

-- Unescapes XML entities
fromEntities :: String -> String
fromEntities ('&':xs) =
  case lookupEntity ent' of
        Just c  -> c : fromEntities rest
        Nothing -> '&' : fromEntities xs
    where (ent, rest) = case break (\c -> isSpace c || c == ';') xs of
                             (zs,';':ys) -> (zs,ys)
                             (zs,    ys) -> (zs,ys)
          ent' = case ent of
                      '#':'X':ys -> '#':'x':ys  -- workaround tagsoup bug
                      '#':_ -> ent
                      _     -> ent ++ ";"

fromEntities (x:xs) = x : fromEntities xs
fromEntities [] = []
