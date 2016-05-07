
module Text.Pandoc.Process (pipeProcess)
where
import System.Process
import System.Exit (ExitCode (..))
import Control.Exception
import System.IO (hClose, hFlush)
import Control.Concurrent (putMVar, takeMVar, newEmptyMVar, forkIO)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL

{- |
Version of 'System.Process.readProcessWithExitCode' that uses lazy bytestrings
instead of strings and allows setting environment variables.

@readProcessWithExitCode@ creates an external process, reads its
standard output and standard error strictly, waits until the process
terminates, and then returns the 'ExitCode' of the process,
the standard output, and the standard error.

If an asynchronous exception is thrown to the thread executing
@readProcessWithExitCode@, the forked process will be terminated and
@readProcessWithExitCode@ will wait (block) until the process has been
terminated.
-}

pipeProcess
    :: Maybe [(String, String)] -- ^ environment variables
    -> FilePath                 -- ^ Filename of the executable (see 'proc' for details)
    -> [String]                 -- ^ any arguments
    -> BL.ByteString            -- ^ standard input
    -> IO (ExitCode,BL.ByteString,BL.ByteString) -- ^ exitcode, stdout, stderr
pipeProcess mbenv cmd args input =
    mask $ \restore -> do
      (Just inh, Just outh, Just errh, pid) <- createProcess (proc cmd args)
                                                   { env     = mbenv,
                                                     std_in  = CreatePipe,
                                                     std_out = CreatePipe,
                                                     std_err = CreatePipe }
      flip onException
        (do hClose inh; hClose outh; hClose errh;
            terminateProcess pid; waitForProcess pid) $ restore $ do
        -- fork off a thread to start consuming stdout
        out <- BL.hGetContents outh
        waitOut <- forkWait $ evaluate $ BL.length out

        -- fork off a thread to start consuming stderr
        err <- BL.hGetContents errh
        waitErr <- forkWait $ evaluate $ BL.length err

        -- now write and flush any input
        let writeInput = do
              unless (BL.null input) $ do
                BL.hPutStr inh input
                hFlush inh
              hClose inh

        writeInput

        -- wait on the output
        waitOut
        waitErr

        hClose outh
        hClose errh

        -- wait on the process
        ex <- waitForProcess pid

        return (ex, out, err)

forkWait :: IO a -> IO (IO a)
forkWait a = do
  res <- newEmptyMVar
  _ <- mask $ \restore -> forkIO $ try (restore a) >>= putMVar res
  return (takeMVar res >>= either (\ex -> throwIO (ex :: SomeException)) return)
