
import Data.Char(toUpper)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

main = do interact (unlines . filter (elem 'a') . lines)


hasElfMagic :: L.ByteString  -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]


isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)





closing = readPrice . (!!4) . LC.split ','

readPrice :: LC.ByteString -> Maybe Int
readPrice str =
    case LC.readInt str of 
        Nothing         -> Nothing
        Just (dollars ,rest) ->
            case L.readInt (LC.tail reset) of
                Nothing         -> Nothin
                Just (cents ,more) -> 
                    Just (dollars * 100 + cents)



highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)





