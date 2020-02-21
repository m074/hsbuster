module Files.Wordlist where

import Data.Maybe
import Control.Exception



readWordlist :: FilePath -> IO (Maybe [String])
readWordlist filename = do
    handle(\(e :: IOException) -> return Nothing) $ do
        text <- readFile filename
        let ll = lines text
        return (Just ll)

