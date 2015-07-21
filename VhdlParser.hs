import System.Directory
import System.Environment
import qualified Data.List as List
import qualified Tokens as Tokens
import Data.Maybe

type Command = String

main = do
  args <- getArgs
  let vhdlFolderPath = fromJust $ List.lookup "path" $ map parseCmdLineArg args
  filePaths <- getDirectoryContents vhdlFolderPath
  let vhdlFilePaths = zipWith (++) (repeat vhdlFolderPath) $ filter (List.isInfixOf ".vhd") filePaths
  xs <- sequence $ map readFile vhdlFilePaths
  sequence(map (print . findEntities . Tokens.alexScanTokens) $ xs)

findEntities :: [String] -> [String]
findEntities [] = []
findEntities (x:xs) = filter (/=[]) $ (if x == "entity" then head xs else []) : findEntities xs

parseCmdLineArg :: String ->(Command,String)
parseCmdLineArg a
  | List.isPrefixOf "--path=" a = ("path",filter (/='"') arg)
  | List.isPrefixOf "--topLevel=" a = ("topLevel",arg)
  | otherwise = ("Invalid Command","")
  where
    arg = tail . snd $ List.break (=='=') a
