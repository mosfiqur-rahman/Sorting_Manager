
module Text.Pandoc.Slides ( getSlideLevel, prepSlides ) where
import Text.Pandoc.Definition

-- | Find level of header that starts slides (defined as the least header
-- level that occurs before a non-header/non-hrule in the blocks).
getSlideLevel :: [Block] -> Int
getSlideLevel = go 6
  where go least (Header n _ _ : x : xs)
                 | n < least && nonHOrHR x = go n xs
                 | otherwise               = go least (x:xs)
        go least (_ : xs) = go least xs
        go least [] = least
        nonHOrHR (Header{}) = False
        nonHOrHR (HorizontalRule) = False
        nonHOrHR _ = True

-- | Prepare a block list to be passed to hierarchicalize.
prepSlides :: Int -> [Block] -> [Block]
prepSlides slideLevel = ensureStartWithH . splitHrule . extractRefsHeader
  where splitHrule (HorizontalRule : Header n attr xs : ys)
                       | n == slideLevel = Header slideLevel attr xs : splitHrule ys
        splitHrule (HorizontalRule : xs) = Header slideLevel nullAttr [Str "\0"] :
                                           splitHrule xs
        splitHrule (x : xs)              = x : splitHrule xs
        splitHrule []                    = []
        extractRefsHeader bs             =
          case reverse bs of
               (Div ("",["references"],[]) (Header n attrs xs : ys) : zs)
                 -> reverse zs ++ (Header n attrs xs : [Div ("",["references"],[]) ys])
               _ -> bs
        ensureStartWithH bs@(Header n _ _:_)
                       | n <= slideLevel = bs
        ensureStartWithH bs              = Header slideLevel nullAttr [Str "\0"] : bs
