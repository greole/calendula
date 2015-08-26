module MiniMarkdown where

{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec
import Control.Applicative (pure, liftA2, (*>),(<*),(<*>),(<$>))

data Token = Head (Int, String)
           | Par [Token]
           | Bold String
           | Emph String
           | ICode String
           | BCode String
           | Url (String, String)
           | List [Token]
           | Stuff String
           | Blockquote [Token]
           | Hrule
           | EndPar
           | Emptym

instance Show Token where
    show (Bold x) = wrap "strong" x
    show (Emph x) = wrap "em" x
    show (Stuff x) = x ++ " "
    show (Head (l, x)) = wrap ("h" ++ show l) (namedWrap "a" "name" x x)
    show (Url (l, x)) = href x l
    show (List x) = wrap "ul" $ wrap "li" $ concat $ map show x
    show (ICode x) = wrap "code" x
    show (BCode x) = wrap "pre" $ wrap "code" x
    show (Par xs) = wrap "p" $ concat $ map show xs
    show Hrule  = "<hr />"
    show EndPar = ""
    show Emptym = ""

type HTML = String
type Markdown = String

makeEmpty _ = Emptym

wrap a b = "<" ++ a ++ ">" ++ b ++ "</" ++ a ++ ">"
namedWrap a b c d  = "<" ++ a ++ " " ++ b ++ "=\"" ++ c ++ "\">" ++ d ++ "</" ++ a ++ ">"
href a b  =  namedWrap "a" "href" a b

{- Block Parsers -}

parseMarkdownBlocks :: Parser [Token]
parseMarkdownBlocks = many $ choice (map try [mhead, hrule, list, bcode, par, endBlock, swl])

mhead :: Parser Token
mhead = fmap Head $ liftA2 (,) numB cont
    where numB = fmap length (many1 (char '#') <* spaces)
          cont = many (noneOf "\n") <* nl

list :: Parser Token
list = fmap List $ between open close parseMarkdownInline
            where open = (oneOf "*-+") <* space
                  close =lookAhead $ try endBlock

hrule :: Parser Token
hrule = do
    string "---" <|> (string "___")
    many (oneOf "-_") <* (char '\n')
    return Hrule

bcode :: Parser Token
bcode =  fmap BCode (blockCode <|> blockCode2)

blockCode = between del del (anyTill del)
        where  del = (count 3 (char '~')) <* nl

blockCode2 :: Parser String
blockCode2 = between open close (anyTill close)
            where open = lookAhead (try (string "    ") <|> try (string "\t"))
                  close = do 
                        try endBlock 
                        (notFollowedBy (string "    ") <|> notFollowedBy (string "\t"))
                        return " "

par = fmap Par $ parseMarkdownInline <* try endBlock

endBlock = do 
    string "\n\n"
    many (char '\n')
    return EndPar

swl = do
    string "\n"
    return EndPar

anyTillEndBlock = anyTill endBlock
anyTill close = many1Till anyChar $ lookAhead $ try $ close
optionalEOL = optional (many nl)
eol = fmap makeEmpty nl
nl = (char '\n')

{- Inline Parser -}

parseMarkdownInline :: Parser [Token]
parseMarkdownInline = many1 (markdownInline <|> mstring)

markdownInline = choice (map try [url, emph, bold, iCode])

url :: Parser Token
url = fmap Url $ liftA2 (,) name url
        where  name = between open close $ delChar "]"
                    where open  = char '[' <* lookAhead (noneOf " *")
                          close = char ']'
               url = between open close $ delChar ")"
                    where open  = char '(' <* lookAhead (noneOf " *")
                          close = char ')'

delChar x = many (noneOf ("\n" ++ x))

emph :: Parser Token
emph = fmap Emph $ between open del $ delChar "*"
        where open = del <* lookAhead (noneOf " *")
              del = char '*'

iCode = fmap ICode $ between del del $ delChar "`"
         where del = char '`'

bold :: Parser Token
bold = fmap Bold $ between open del $ delChar "*"
            where open = del <* notFollowedBy space
                  del = count 2 $ char '*'

mstring :: Parser Token
mstring = fmap Stuff $ many1Till (noneOf "\n") newToken
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
    let res' = concat $ map show res
    return res'

writeHTMLString :: Markdown -> HTML
writeHTMLString inp = stripEither $ parse2HTML (inp ++ "\n\n")

stripEither (Right x) = x
stripEither (Left x) = show x
