
import System.Process
import Distribution.Simple.LocalBuildInfo
import Distribution.Package
import Distribution.Version
import Data.List (intercalate)

main = do
  setupConfig' <- readFile "dist/setup-config"
  let setupConfig = read $ unlines $ drop 1 $ lines setupConfig'
  let (Just (ComponentLocalBuildInfo { componentPackageDeps = deps })) = libraryConfig setupConfig
  let packageSpecs = map (toPackageSpec . snd) deps
  let args = ["-optP-include", "-optP../dist/build/autogen/cabal_macros.h","-cpp","-I../dist/build/autogen","-i../dist/build/autogen"] ++ concatMap (\p -> ["-package",p]) packageSpecs
  print args
  ph <- runProcess "ghci" args (Just "src") Nothing Nothing Nothing Nothing
  waitForProcess ph

toPackageSpec pkg = pkgN ++ "-" ++ pkgV
  where (PackageName pkgN) = pkgName pkg
        pkgV = intercalate "." $ map show $ versionBranch $ pkgVersion pkg
