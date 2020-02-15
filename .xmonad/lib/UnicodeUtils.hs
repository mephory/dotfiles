module UnicodeUtils (
    writeFileUtf8,
    appendFileUtf8
  ) where

import System.IO

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 f s = do
    h <- openFile f WriteMode
    hSetBinaryMode h True
    hPutStr h s
    hFlush h
    hClose h

appendFileUtf8 :: FilePath -> String -> IO ()
appendFileUtf8 f s = do
    h <- openFile f AppendMode
    hSetBinaryMode h True
    hPutStr h s
    hFlush h
    hClose h
