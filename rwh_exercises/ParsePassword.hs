import Control.Monad(when)
import Data.List(lookup)
import Data.List.Split(splitOn)
import Data.Maybe(isJust, fromJust)
import System.Exit
import System.Environment(getArgs)

main = do
  args <- getArgs

  when (length args /= 2) $ do
    putStrLn "Usage - ParsePassword {uid} {password file path}"
    exitFailure

  let uid      = read (args !! 0)
  let filePath = args !! 1

  content <- readFile filePath

  let username = findByUID content uid
  case username of
    Just x -> putStrLn x
    _      -> putStrLn "UID not found."



type UIDWithUsername = (Integer, String)

findByUID :: String -> Integer -> Maybe String
findByUID content uid = lookup uid . parseLines . lines $ content

parseLines :: [String] -> [UIDWithUsername]
parseLines = filterInvalidRecords . map parseLine
  where
    filterInvalidRecords :: [Maybe UIDWithUsername] -> [UIDWithUsername]
    filterInvalidRecords = map fromJust . filter isJust

parseLine :: String -> Maybe UIDWithUsername
parseLine s = 
  let splitString = splitOn ":" s in
      case splitString of
        (userName:stuff:uid:xxs) -> Just ((read uid), userName)
        _                        -> Nothing

