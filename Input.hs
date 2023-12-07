module Input (
    getLines
) where

import System.IO (isEOF)

getLines :: IO [String]
getLines = do
    eof <- isEOF
    if eof then return []
    else do
        l <- getLine
        (l:) <$> getLines

