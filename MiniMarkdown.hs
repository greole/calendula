module MiniMarkdown where

{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.ParserCombinators.Parsec (Parser)
import Text.Parsec
import Text.Regex.Posix ((=~))
import Control.Applicative (liftA2, (*>),(<*),(<*>),(<$>))

data Token = Head (Int, String)
           | Bold String
           | Emph String
           | ICode String
           | BCode String
           | Url (String, String)
           | List String
           | Stuff String
           | Emptym
           deriving Show

makeEmpty _ = Emptym

token2Html :: Token -> String
token2Html (Bold x) = wrap "strong" x
token2Html (Emph x) = wrap "em" x
token2Html (Stuff x) = x
token2Html (Head (l, x)) = wrap ("h" ++ show l) x
token2Html (Url (l, x)) = "<a href=\"" ++ x ++ "\">" ++  l ++ "</a>"
token2Html (List x) = wrap "li" x
token2Html (ICode x) = wrap "code" x
token2Html (BCode x) = wrap "pre" $ wrap "code" x
token2Html Emptym = ""

wrapp a b = wrap "p" $ wrap a b
wrap a b = "<" ++ a ++ ">" ++ b ++ "</" ++ a ++ ">"

mhead :: Parser Token
mhead = fmap Head $ liftA2 (,) numB cont
    where numB = fmap length (many (char '\n') *> many1 (char '#') <* spaces)
          cont = many (noneOf "\n") <* lookAhead (char '\n')

url :: Parser Token
url = fmap Url $ liftA2 (,) name url
        where  name = between open close cont
                    where open  = char '[' <* lookAhead (noneOf " *")
                          close = char ']'
                          cont  = many (noneOf "\n]")
               url = between open close cont
                    where open  = char '(' <* lookAhead (noneOf " *")
                          close = char ')'
                          cont  = many (noneOf "\n)")

emph :: Parser Token
emph = fmap Emph $ between open close cont
        where cont = many (noneOf "\n*")
              open = char '*' <* lookAhead (noneOf " *")
              close = char '*'

inlineCode :: Parser Token
inlineCode = fmap ICode $ between del del cont
        where cont = many (noneOf "`\n")
              del = char '`'

blockCode :: Parser Token
blockCode = fmap BCode $ between open close cont
        where cont = manyTill anyChar (try (lookAhead close))
              open = (count 3 (char '~')) <* char '\n'
              close = (count 3 (char '~')) <* char '\n'

blockCode2 :: Parser Token
blockCode2 = fmap BCode $ between open close cont
        where cont = manyTill anyChar (try (lookAhead close))
              open = string "\n" <* lookAhead (try (string "   ") <|>
                                               try (string "\t"))
              close = string "\n\n"

bold :: Parser Token
bold = fmap Bold $ between open close cont
        where cont = many (noneOf "\n*")
              open = count 2 $ char '*' <* notFollowedBy space
              close = count 2 $ char '*'

list :: Parser Token
list =  fmap List $ between open close cont
        where cont = many (noneOf "\n")
              open = (oneOf "*-+") <* space
              close = char '\n'

mempty :: Parser Token
mempty =  fmap makeEmpty $ string "EOF"

stuff :: Parser Token
stuff = fmap Stuff $ manyTill contd newToken
        where newToken = lookAhead (choice (map try [emph, bold, list, url,
                                     mhead, inlineCode, blockCode, blockCode2,
                                     mempty]))
              contd = anyChar

parseMarkdownTokens :: Parser [Token]
parseMarkdownTokens = many $ choice [try url, try emph, try bold,
                                    try mhead, try inlineCode,
                                    try blockCode, try list, try blockCode2,
                                    try mempty, stuff]

parseMarkdown :: String -> Either ParseError [Token]
parseMarkdown inp = parse parseMarkdownTokens "(unknown)" inp

parse2HTML inp = do
    res <- parseMarkdown inp
    let res' = concat $ map token2Html res
    return res'

writeHTMLString :: String -> String
writeHTMLString inp = stripEither $ parse2HTML (inp ++ "EOF")
    where stripEither (Right x) = x
          stripEither (Left x) = show x
