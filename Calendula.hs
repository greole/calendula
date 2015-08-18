import Data.List (isInfixOf)
import Data.String.Utils (replace)
import System.FilePath.Find
import System.FilePath.Posix (makeRelative, takeDirectory)
import System.Directory (getCurrentDirectory, copyFile)
import System.Environment (getExecutablePath)
import Text.Regex.Posix
import Text.Pandoc
import Control.Monad
import Control.Monad (zipWithM_)

htmlHeader = "<!DOCTYPE html><html><body><head><link rel=\"stylesheet\" \
     \ type=\"text/css\" href=\"style.css\"></head><div class=\"outer\"> \
     \ <div class=\"toc\"></div><div class=\"wrapper\">"

htmlFooter = "</div></div><script src=\"jquery-2.1.4.min.js\"></script><script\
   \ src=\"toc.min.js\"></script><script>$('.toc').toc({});</script></body></html>"

notGit :: FindClause Bool
notGit = directory /=? ".git"

-- First scan complete file system for markdown files
buildIndex :: FilePath -> IO [FilePath]
buildIndex = find notGit (fileType ==? RegularFile &&? extension ==? ".md")

concatMarkdown :: FilePath -> [FilePath] -> IO ()
concatMarkdown outFile files = do 
        writeFile outFile htmlHeader
        forM_ files $ \infile -> do
            contents <- readFile infile
            appendFile outFile $ markdownToHtml $ "#" ++  fileToHeader infile 
            appendFile outFile $ markdownToHtml contents 
        appendFile outFile htmlFooter

fileToHeader :: String -> String
fileToHeader x = if (x =~ "index.md" :: Bool)
        then (x =~ "[A-Za-z ]*/index.md" :: String) =~ "[A-Za-z ]*" :: String
        else (x =~ "[A-Za-z ]*.md" :: String) =~ "[A-Za-z ]*" :: String

markdownToHtml :: String -> String
markdownToHtml = (writeHtmlString def {writerReferenceLinks = True}) . readMarkdown def

main = do
    let targetDir = "/tmp/Calendula/"
    exec <- getExecutablePath
    cwd <- getCurrentDirectory
    index <- buildIndex cwd
    let assets = ["style.css", "jquery-2.1.4.min.js", "toc.min.js"]
    let assetsSrc = map ((takeDirectory exec ++ "/assets/") ++ ) assets
    let assetsDst = map (targetDir ++ ) assets
    putStrLn $ show assets
    zipWithM_ copyFile assetsSrc assetsDst
    concatMarkdown (targetDir ++ "index.html") index
