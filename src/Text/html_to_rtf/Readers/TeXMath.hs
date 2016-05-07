
module Text.Pandoc.Readers.TeXMath ( texMathToInlines ) where

import Text.Pandoc.Definition
import Text.TeXMath

-- | Converts a raw TeX math formula to a list of 'Pandoc' inlines.
-- Defaults to raw formula between @$@ or @$$@ characters if entire formula
-- can't be converted.
texMathToInlines :: MathType
             -> String    -- ^ String to parse (assumes @'\n'@ line endings)
             -> [Inline]
texMathToInlines mt inp =
  case writePandoc dt `fmap` readTeX inp of
       Right (Just ils)  -> ils
       _                 -> [Str (delim ++ inp ++ delim)]
    where (dt, delim) = case mt of
                             DisplayMath -> (DisplayBlock, "$$")
                             InlineMath  -> (DisplayInline, "$")

