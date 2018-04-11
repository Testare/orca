import Orca.Reader(orcaReadImageIO)
import Orca.Reader.Data
import Orca.Reader.Types

main :: IO ()
main = do
    putStrLn "Specify file to read:"
    fp <- getLine
    k <- orcaReadImageIO defaultParams fp
    maybe (putStrLn "File not found") putStrLn k