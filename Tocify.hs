module Tocify where


{-# LANGUAGE NoMonomorphismRestriction #-}

import MiniMarkdown
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec
import Control.Applicative (pure, liftA2, (*>),(<*),(<*>),(<$>))

data Toc = TocHead (Int, String) [Toc]
         | EmptyToc

instance Show Toc where
    show (TocHead (l, x) toc) =
               namedWrap "div" "class" ("topwrapper" ++ (show l)) $
               namedWrap "div" "class" "cnt" (href ("#"++x) x) ++ (subTocs l $ filtered toc)
                  where filtered x = filter (/= EmptyToc) x
                        subTocs l s
                            | length s >= 1 = namedWrap "ul" "class" ("subtocs" ++ show l)$
                                              unlines (map ((wrap "li") . show) s)
                            | otherwise = ""
    show (EmptyToc) = ""

instance Eq Toc where
    (==) EmptyToc EmptyToc = True
    (==) _ _ = False

tochead :: Parser Toc
tochead = do
    many tocEL
    start <- many1 (char '#')
    let num = length start
    spaces
    cont <- many (noneOf "\n")
    stopdescent <- (lookAhead (try (isLevel num))) <|> try onlyNewlines <|> pure False
    subtocs <- getsubtocs stopdescent
    optional (many tocEL)
    return (TocHead (num, cont) subtocs)

getsubtocs :: Bool -> Parser [Toc]
getsubtocs False = many (try tochead)
getsubtocs True = pure []

isLevel start = do
    optional (many (noneOf "#"))
    string (replicate start '#')
    spaces
    many1 alphaNum
    return True

onlyNewlines :: Parser Bool
onlyNewlines = do
    skipMany newline
    notFollowedBy anyToken
    return True

tocEL :: Parser Toc
tocEL = do
    many (noneOf "#\n") <* char '\n'
    return EmptyToc

parseMarkdownTOC :: String -> Either ParseError [Toc]
parseMarkdownTOC inp = parse (many tochead) "(unknown)" inp

parseTOC2HTML inp = do
    res <- parseMarkdownTOC inp
    let res' = concat $ map show res
    return res'

tocify :: Markdown -> HTML
tocify inp = stripEither $ parseTOC2HTML (inp ++ "\n\n")
