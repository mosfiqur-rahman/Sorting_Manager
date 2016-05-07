{-# LANGUAGE CPP #-}

module Text.Pandoc.UTF8 ( readFile
                        , writeFile
                        , getContents
                        , putStr
                        , putStrLn
                        , hPutStr
                        , hPutStrLn
                        , hGetContents
                        , toString
                        , fromString
                        , toStringLazy
                        , fromStringLazy
                        , encodePath
                        , decodeArg
                        )

where

import System.IO hiding (readFile, writeFile, getContents,
                          putStr, putStrLn, hPutStr, hPutStrLn, hGetContents)
import Prelude hiding (readFile, writeFile, getContents, putStr, putStrLn)
import qualified System.IO as IO
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

readFile :: FilePath -> IO String
readFile f = do
  h <- openFile (encodePath f) ReadMode
  hGetContents h

writeFile :: FilePath -> String -> IO ()
writeFile f s = withFile (encodePath f) WriteMode $ \h -> hPutStr h s

getContents :: IO String
getContents = hGetContents stdin

putStr :: String -> IO ()
putStr s = hPutStr stdout s

putStrLn :: String -> IO ()
putStrLn s = hPutStrLn stdout s

hPutStr :: Handle -> String -> IO ()
hPutStr h s = hSetEncoding h utf8 >> IO.hPutStr h s

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h s = hSetEncoding h utf8 >> IO.hPutStrLn h s

hGetContents :: Handle -> IO String
hGetContents = fmap toString . B.hGetContents
-- hGetContents h = hSetEncoding h utf8_bom
--                   >> hSetNewlineMode h universalNewlineMode
--                   >> IO.hGetContents h

-- | Drop BOM (byte order marker) if present at beginning of string.
-- Note that Data.Text converts the BOM to code point FEFF, zero-width
-- no-break space, so if the string begins with this  we strip it off.
dropBOM :: String -> String
dropBOM ('\xFEFF':xs) = xs
dropBOM xs = xs

filterCRs :: String -> String
filterCRs ('\r':'\n':xs) = '\n': filterCRs xs
filterCRs ('\r':xs) = '\n' : filterCRs xs
filterCRs (x:xs) = x : filterCRs xs
filterCRs []     = []

-- | Convert UTF8-encoded ByteString to String, also
-- removing '\r' characters.
toString :: B.ByteString -> String
toString = filterCRs . dropBOM . T.unpack . T.decodeUtf8

fromString :: String -> B.ByteString
fromString = T.encodeUtf8 . T.pack

-- | Convert UTF8-encoded ByteString to String, also
-- removing '\r' characters.
toStringLazy :: BL.ByteString -> String
toStringLazy = filterCRs . dropBOM . TL.unpack . TL.decodeUtf8

fromStringLazy :: String -> BL.ByteString
fromStringLazy = TL.encodeUtf8 . TL.pack

encodePath :: FilePath -> FilePath
decodeArg :: String -> String
#if MIN_VERSION_base(4,4,0)
encodePath = id
decodeArg = id
#else
encodePath = B.unpack . fromString
decodeArg = toString . B.pack
#endif
