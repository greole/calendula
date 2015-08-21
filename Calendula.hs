import Data.List (isInfixOf)
import Data.String.Utils (replace)
import System.FilePath.Find (
    fileType, find, (==?), directory, (/=?), FileType(..), FindClause)
import System.FilePath.Posix (makeRelative, takeDirectory, takeFileName) 
import System.Directory (getCurrentDirectory, copyFile, createDirectoryIfMissing)
import System.Environment (getExecutablePath, getArgs)
import Text.Regex.Posix ((=~))
import Control.Monad (zipWithM_, forM_, forM)
import Paths_calendula
import MiniMarkdown

notGit :: FindClause Bool
notGit = directory /=? ".git"

-- First scan complete file system for markdown files
buildIndex :: FilePath -> IO [FilePath]
buildIndex = find notGit (fileType ==? RegularFile)

concatMarkdown :: FilePath -> [FilePath] -> IO ()
concatMarkdown dst files = mapM_ (concatFile transform dst) $ filter isMDfile files
    where transform x = wrapDiv . writeHTMLString . (incHeaderLevel x)

concatFile :: (Int -> String -> String) -> FilePath -> FilePath -> IO ()
concatFile parse dst src = do
            contents <- readFile src
            level <- subLevel src
            appendFile dst $ parse level contents

subLevel :: FilePath -> IO Int
subLevel x = do
       cwd <- getCurrentDirectory
       return $ (count '/' (makeRelative cwd x)) + (index x) 
            where count c = length . filter (==c)
                  index :: FilePath -> Int
                  index c = if c =~ "index.md" :: Bool
                            then 0
                            else 1

incHeaderLevel :: Int -> String -> String
incHeaderLevel i = replace "#" $ replicate (i+1) '#'

isMDfile :: String -> Bool
isMDfile = (=~ ".md")

isContent :: String -> Bool
isContent = not . (=~ ".md|.html")

wrapDiv :: String -> String
wrapDiv x = "<div class=\"article\">" ++ x ++ "</div>"

main = do
    args <- getArgs 
    let targetDir = args !! 0
    createDirectoryIfMissing True targetDir
    cwd <- getCurrentDirectory
    index <- buildIndex cwd
    let assets = ["footer.html", "index.html", "style.css", "jquery.min.js", "toc.min.js"]
    assetsSrc <- forM (map ("assets/" ++) assets) getDataFileName 
    let assetsDst = map (targetDir ++ ) assets
    let mediaSrc = filter isContent index 
    let mediaDst = map ((targetDir ++) . takeFileName) mediaSrc
    zipWithM_ copyFile assetsSrc assetsDst
    zipWithM_ copyFile mediaSrc mediaDst
    concatMarkdown (targetDir ++ "index.html") index
    concatFile (\ _ s -> s) (targetDir ++ "index.html") (targetDir ++ "footer.html")
