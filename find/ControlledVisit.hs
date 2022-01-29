import Data.Time (UTCTime)
import System.IO (IOMode(..), hClose, hFileSize, openFile,withFile)
import Control.Exception (bracket, handle)

import System.Directory (Permissions(..), getModificationTime, getPermissions,getDirectoryContents)
import Control.Monad ( liftM, forM )
import System.FilePath ( (</>) )

data Info = Info {
      infoPath :: FilePath
    , infoPerms :: Maybe Permissions
    , infoSize :: Maybe Integer
    , infoModTime :: Maybe UTCTime
    } deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = Just `liftM` act


getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (withFile path ReadMode (hFileSize))
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

traverse' :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse' order path = do
    names <- getUsefulContents path
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info -> do
      if isDirectory info && infoPath info /= path
        then traverse' order (infoPath info)
        else return [info]



getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

