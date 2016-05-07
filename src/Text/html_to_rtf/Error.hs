{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Text.Pandoc.Error (PandocError(..), handleError) where

import Text.Parsec.Error
import Text.Parsec.Pos hiding (Line)
import Text.Pandoc.Compat.Except
import GHC.Generics (Generic)
import Data.Generics (Typeable)
import Control.Exception (Exception)

type Input = String

data PandocError = -- | Generic parse failure
                   ParseFailure String
                 -- | Error thrown by a Parsec parser
                 | ParsecError Input ParseError
                 deriving (Show, Typeable, Generic)

instance Exception PandocError

instance Error PandocError where
  strMsg = ParseFailure


-- | An unsafe method to handle `PandocError`s.
handleError :: Either PandocError a -> a
handleError (Right r) = r
handleError (Left err) =
  case err of
    ParseFailure string -> error string
    ParsecError input err' ->
        let errPos = errorPos err'
            errLine = sourceLine errPos
            errColumn = sourceColumn errPos
            theline = (lines input ++ [""]) !! (errLine - 1)
        in  error $ "\nError at " ++ show  err' ++ "\n" ++
                theline ++ "\n" ++ replicate (errColumn - 1) ' ' ++
                "^"

