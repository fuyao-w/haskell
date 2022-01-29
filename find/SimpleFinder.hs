import RecursiveContents (getRecursiveContents)
import System.FilePath (takeExtension,FilePath)
simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)

