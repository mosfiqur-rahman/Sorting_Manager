
module Text.Pandoc.Readers.Native ( readNative ) where

import Text.Pandoc.Definition
import Text.Pandoc.Shared (safeRead)

import Text.Pandoc.Error

-- | Read native formatted text and return a Pandoc document.
-- The input may be a full pandoc document, a block list, a block,
-- an inline list, or an inline.  Thus, for example,
--
-- > Str "hi"
--
-- will be treated as if it were
--
-- > Pandoc nullMeta [Plain [Str "hi"]]
--
readNative :: String      -- ^ String to parse (assuming @'\n'@ line endings)
           -> Either PandocError Pandoc
readNative s = maybe (Pandoc nullMeta <$> readBlocks s) Right (safeRead s)

readBlocks :: String -> Either PandocError [Block]
readBlocks s = maybe ((:[]) <$> readBlock s) Right (safeRead s)

readBlock :: String -> Either PandocError Block
readBlock s = maybe (Plain <$> readInlines s) Right (safeRead s)

readInlines :: String -> Either PandocError [Inline]
readInlines s = maybe ((:[]) <$> readInline s) Right (safeRead s)

readInline :: String -> Either PandocError Inline
readInline s = maybe (Left . ParseFailure $ "Could not read: " ++ s) Right (safeRead s)

