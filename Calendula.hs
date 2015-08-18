import Data.List (isInfixOf)
import Data.String.Utils (replace)
import System.FilePath.Find (
    fileType, find, (==?), directory, (/=?), FileType(..), FindClause)
import System.FilePath.Posix (makeRelative, takeDirectory, takeFileName)
import System.Directory (getCurrentDirectory, copyFile)
import System.Environment (getExecutablePath)
import Text.Regex.Posix ((=~))
import Text.Pandoc
import Control.Monad (zipWithM_, forM_, forM)
import Paths_calendula

htmlHeader = "<!DOCTYPE html><html><body><head><link rel=\"stylesheet\" \
     \ type=\"text/css\" href=\"style.css\"></head><div class=\"outer\"> \
     \ <div class=\"toc\"></div><div class=\"wrapper\">"

htmlFooter = "</div></div><script src=\"jquery.min.js\"></script><script\
   \ src=\"toc.min.js\"></script><script>$('.toc').toc({'selectors': 'h1,h2,h3,h4'});</script></body></html>"

notGit :: FindClause Bool
notGit = directory /=? ".git"

-- First scan complete file system for markdown files
buildIndex :: FilePath -> IO [FilePath]
buildIndex = find notGit (fileType ==? RegularFile)

concatMarkdown :: FilePath -> [FilePath] -> IO ()
concatMarkdown outFile files = do 
        writeFile outFile htmlHeader
        forM_ (filter isMDfile files) $ \infile -> do
            contents <- readFile infile
            level <- subLevel infile
            appendFile outFile $ wrapDiv . markdownToHtml $ 
                incHeaderLevel level contents
        appendFile outFile htmlFooter

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

markdownToHtml :: String -> String
markdownToHtml = (writeHtmlString def {writerReferenceLinks = True}) . readMarkdown def

wrapDiv :: String -> String
wrapDiv x = "<div class=\"article\">" ++ x ++ "</div>"

main = do
    let targetDir = "/tmp/Calendula/"
    cwd <- getCurrentDirectory
    index <- buildIndex cwd
    let assets = ["style.css", "jquery.min.js", "toc.min.js"]
    assetsSrc <- forM (map ("assets/" ++) assets) getDataFileName 
    putStrLn (show assetsSrc)
    let assetsDst = map (targetDir ++ ) assets
    let mediaSrc = filter isContent index 
    let mediaDst = map ((targetDir ++) . takeFileName) mediaSrc
    zipWithM_ copyFile assetsSrc assetsDst
    zipWithM_ copyFile mediaSrc mediaDst
    concatMarkdown (targetDir ++ "index.html") index
