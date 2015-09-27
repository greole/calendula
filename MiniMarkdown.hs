module MiniMarkdown where

{-# LANGUAGE NoMonomorphismRestriction #-}

import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec
import Control.Applicative (pure, liftA2, (*>),(<*),(<*>),(<$>))
import Data.List (intersperse)
import Debug.Trace

data Token = Head (Int, String)
           | Par [Token]
           | Bold String
           | Emph String
           | ICode String
           | BCode [Token]
           | Url (String, String)
           | List [Token]
           | MString String
           | Blockquote [Token]
           | Hrule
           | EmptyLine
           | LeadingWs
           | Ws
           | LeadingEmptyLine
           | InlineNL
           | EndPar
           | Emptym
           deriving Show

--instance Show Token where
htmlshow (Bold x)      = wrap "strong" x
htmlshow (Emph x)      = wrap "em" x
htmlshow (MString x)   = x
htmlshow (Head (l, x)) = wrap ("h" ++ show l) (namedWrap "a" "name" x x)
htmlshow (Url (l, x))  = href x l
htmlshow (List x)      = wrap "ul" $ wrap "li" $ concat $ map htmlshow x
htmlshow (ICode x)     = wrap "code" x
htmlshow (BCode x)     = wrap "pre" $ wrap "code" $ concat $
                            intersperse "\n" $ map htmlshow x
htmlshow (Par xs)         = wrap "p" $ concat $ map htmlshow xs
htmlshow Hrule            = "<hr />"
-- Empty Elems
htmlshow Ws               = " "
htmlshow EndPar           = ""
htmlshow Emptym           = ""
htmlshow EmptyLine        = ""
htmlshow InlineNL         = " "
htmlshow LeadingWs        = ""
htmlshow LeadingEmptyLine = ""

type HTML = String
type Markdown = String

wrap a b = concat ["<",a,">",b,"</",a,">"]
namedWrap a b c d  = concat ["<", a," ",b,"=\"",c,"\">",d,"</",a,">"]
href a b  =  namedWrap "a" "href" a b

{-  Block Parsers
    -------------
    Tries to find a markdown block, if none of the block parsers
    is succesful all empty lines are consumend (handles \n at the
    beginning or end) -}

parseMarkdownBlocks :: Parser [Token]
parseMarkdownBlocks = many $ choice (map try bParser)
    where bParser = [wsp,lineOfWsp,swl,headl,hrule,list,bcode,par,endBlock]

-- consume whitespaces at beginning of line if less than 3
wsp :: Parser Token
wsp = do
    upTo 3 ws (oneOf " \t") <|>
          upTo 2 ws (oneOf "\t") <|>
          upTo 1 ws (oneOf "\t")
    return LeadingWs

lineOfWsp = do
    many1 ws
    try $ notFollowedBy (noneOf " \t")
    lookAhead nl
    return EmptyLine

upTo n p end = do
    try $ count n p
    notFollowedBy end

headl :: Parser Token
headl = Head <$> liftA2 (,) numB cont
    where numB = fmap length (many1 (char '#') <* many1 space)
          cont = many (noneOf "\n") <* nl

list :: Parser Token
list = List <$> between open close parseMarkdownInline
            where open  = (oneOf "*-+") <* space
                  close = lookAhead $ try endBlock

hrule :: Parser Token
hrule = rule '*' <|> rule '-' <|> rule '_'
    where rule :: Char -> Parser Token
          rule x = do
              count 3 (char x)
              many (char x) <* nl
              return Hrule

bcode :: Parser Token
bcode =  fmap BCode (blockCode <|> blockCode2)

blockCode :: Parser [Token]
blockCode = between del del (many purestring)
        where del = (count 3 (char '~')) <* nl

blockCode2 :: Parser [Token]
blockCode2 = between open close (many purestring)
            where open = try (string "    ") <|> try (string "\t") -- NOTE how to throw a Failure
                  close = do
                        try $ nl
                        (notFollowedBy (string "    ") <|> notFollowedBy (string "\t"))
                        return " "

-- Parse a string skipping initial ws including \n
purestring :: Parser Token
purestring = do
    notFollowedBy $ string "~~~\n"
    optional (many1 (oneOf " \t"))
    cnt <- many1Till (noneOf "\n") nl
    return $ MString cnt

par :: Parser Token
par = Par <$> parseMarkdownInline <* try endBlock

endBlock :: Parser Token
endBlock = string "\n\n" *> nl *> return EndPar

swl :: Parser Token
swl = nl *> notFollowedBy ws *> return LeadingEmptyLine

anyTillEndBlock :: Parser String
anyTillEndBlock = anyTill endBlock

anyTill :: Parser Token -> Parser String
anyTill close = many1Till anyChar $ lookAhead $ try $ close

-- optEOL = optional (many nl)

eol :: Parser Token
eol = nl *> pure Emptym

nl :: Parser Char
nl = char '\n'

ws :: Parser Char
ws = char ' '

{- Inline Parser -}

parseMarkdownInline :: Parser [Token]
parseMarkdownInline = many1 (markdownInline <|> mstring)

markdownInline = choice (map try [url, emph, bold, iCode, ws', inlineNL])

ws' :: Parser Token
ws' = do
    try $ many1 $ oneOf " \t"
    lookAhead $ try $ noneOf " \t"
    return Ws

inlineNL :: Parser Token
inlineNL = nl *> notFollowedBy nl *> return InlineNL

url :: Parser Token
url = Url <$> liftA2 (,) name url
    where name = between open close $ delChar "]"
               where open  = char '[' <* lookAhead (noneOf " *")
                     close = char ']'
          url = between open close $ delChar ")"
               where open  = char '(' <* lookAhead (noneOf " *")
                     close = char ')'

delChar :: String -> Parser String
delChar x = many (noneOf ("\n" ++ x))

emph :: Parser Token
emph = Emph <$> (between open del $ delChar "*")
        where open = del <* lookAhead (noneOf " *")
              del  = char '*'

iCode = ICode <$> (between del del $ delChar "`")
         where del = char '`'

bold :: Parser Token
bold = Bold <$> (between open del $ delChar "*")
            where open = del <* notFollowedBy space
                  del = count 2 $ char '*'

mstring :: Parser Token
mstring = MString <$> many1Till (noneOf " \t\n") newToken
        where newToken = lookAhead markdownInline <|> lookAhead eol

many1Till p end = do
    notFollowedBy end
    p1 <- p
    ps <- manyTill p end
    return (p1:ps)

{- Parser -}

parseMarkdown :: String -> Either ParseError [Token]
parseMarkdown inp = parse parseMarkdownBlocks "(unknown)" inp

parse2HTML inp = do
    res <- parseMarkdown inp
    let res' = concat $ map htmlshow res
    return res'

writeHTMLString :: Markdown -> HTML
writeHTMLString inp = stripEither $ parse2HTML (inp ++ "\n\n")

stripEither (Right x) = x
stripEither (Left x) = show x
