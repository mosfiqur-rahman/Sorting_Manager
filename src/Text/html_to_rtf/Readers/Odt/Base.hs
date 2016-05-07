{-# LANGUAGE PatternGuards #-}



module Text.Pandoc.Readers.Odt.Base where

import           Text.Pandoc.Readers.Odt.Generic.XMLConverter
import           Text.Pandoc.Readers.Odt.Namespaces

type OdtConverterState s = XMLConverterState Namespace s

type XMLReader     s a b = FallibleXMLConverter Namespace s a b

type XMLReaderSafe s a b =         XMLConverter Namespace s a b

