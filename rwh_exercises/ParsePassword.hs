import Control.Monad(when)
import Data.List.Split(splitOn)
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

findByUID :: String -> Integer -> Maybe String
findByUID content uid = findUsernameByUID uid (map parseLine (lines content))

type UIDWithUsername = (Integer, String)

parseLine :: String -> UIDWithUsername
parseLine s = 
  let splitString = splitOn ":" s in
      case splitString of
        (userName:stuff:uid:xxs) -> ((read uid), userName)
        _                        -> ((-10000), "these are not the droids you're looking for")

findUsernameByUID :: Integer -> [UIDWithUsername] -> Maybe String
findUsernameByUID uid list = foldr (\(userID, username) acc -> if userID == uid then (Just username) else acc) Nothing list

