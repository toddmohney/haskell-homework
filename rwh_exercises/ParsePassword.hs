import Control.Monad(when)
import Data.List.Split(splitOn)
import Data.Maybe(fromJust)
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
findByUID content uid = findUsernameByUID uid . parseLines . lines $ content

parseLines :: [String] -> [Maybe UIDWithUsername]
parseLines = map parseLine

parseLine :: String -> Maybe UIDWithUsername
parseLine s = 
  let splitString = splitOn ":" s in
      case splitString of
        (userName:stuff:uid:xxs) -> Just ((read uid), userName)
        _                        -> Nothing

findUsernameByUID :: Integer -> [Maybe UIDWithUsername] -> Maybe String
findUsernameByUID uid list = foldr (\tuple acc -> if (checkMatch tuple uid) then (Just (getUsername (fromJust tuple))) else acc) Nothing list

checkMatch :: Maybe UIDWithUsername -> Integer -> Bool
checkMatch (Just (uid, _)) uidToMatch = uid == uidToMatch
checkMatch Nothing _                        = False

getUsername :: UIDWithUsername -> String
getUsername (_, username) = username

