import System.Environment
import Control.Monad
import Control.Applicative
import Control.Concurrent
import System.IO

import Network.OnDemandSSHTunnel

main :: IO ()
main = do
    tuns <- unwords <$> getArgs
    let cfg = Config $ read tuns
    putStrLn $ "cfg:\n" ++ prettyConfig cfg
    hFlush stdout
    setupConfiguration cfg
    putStrLn "enter to exit..."
    _ <- getLine
    return ()
