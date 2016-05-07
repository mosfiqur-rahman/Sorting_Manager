
import Text.Pandoc
import Criterion.Main
import Criterion.Types (Config(..))
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import Text.Pandoc.Error

readerBench :: Pandoc
            -> (String, ReaderOptions -> String -> IO (Either PandocError Pandoc))
            -> Maybe Benchmark
readerBench doc (name, reader) =
  case lookup name writers of
       Just (PureStringWriter writer) ->
         let inp = writer def{ writerWrapText = WrapAuto} doc
         in return $ bench (name ++ " reader") $ nfIO $
                 (fmap handleError <$> reader def{ readerSmart = True }) inp
       _ -> trace ("\nCould not find writer for " ++ name ++ "\n") Nothing

writerBench :: Pandoc
            -> (String, WriterOptions -> Pandoc -> String)
            -> Benchmark
writerBench doc (name, writer) = bench (name ++ " writer") $ nf
    (writer def{ writerWrapText = WrapAuto }) doc

main :: IO ()
main = do
  inp <- readFile "tests/testsuite.txt"
  let opts = def{ readerSmart = True }
  let doc = handleError $ readMarkdown opts inp
  let readers' = [(n,r) | (n, StringReader r) <- readers]
  let readerBs = mapMaybe (readerBench doc)
                 $ filter (\(n,_) -> n /="haddock") readers'
  let writers' = [(n,w) | (n, PureStringWriter w) <- writers]
  let writerBs = map (writerBench doc)
                 $ writers'
  defaultMainWith defaultConfig{ timeLimit = 6.0 }
    (writerBs ++ readerBs)
