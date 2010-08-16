module SSH.Config
  ( Config(..)
  , Section(..)
  , HostOption(..)
  , parser
  )
where
import Char
import Control.Applicative hiding(many, (<|>))
import Control.Monad (ap)
import Text.ParserCombinators.Parsec hiding(space)

{- REPRESENTATION -}

data Config = Config { sections :: [Section] }
  deriving (Show)

data Section =
    HostSection { names :: [String]
                , options :: [HostOption]
                }
  deriving (Show)

data HostOption =
    HostName String
  | UnknownOption { keyword :: String, rest :: String }
  deriving (Show)


{- PARSER -}

parser :: CharParser st Config
parser = configP

configP :: CharParser st Config
configP = Config <$> hostSectionP `swimmingIn` blankOrComment <* eof

hostSectionP :: CharParser st Section
hostSectionP = HostSection <$>
    hostHeaderP <*> many1 hostOptionP

hostHeaderP :: CharParser st [String]
hostHeaderP = line $ string "Host" >> many1 (space *> hostNameP)

hostOptionP :: CharParser st HostOption
hostOptionP = line $ (do
    kw <- many1 letter <* space
    case kw of "HostName" -> HostName <$> hostNameP
               _ -> UnknownOption kw <$> many (noneOf "\n")
    <?> "config option")

hostNameP :: CharParser st String
hostNameP = many1 $ satisfy $ not . isSpace

blankOrComment :: CharParser st String
blankOrComment = comment <|> one newline

comment :: CharParser st String
comment = line $ (
    char '#' *> spaces *> many (noneOf "\n") <?> "comment")

-- override 'space' in Text.ParserCombinators.Parsec which matches any whitespace
space :: CharParser st Char
space = char ' '

line :: CharParser st a -> CharParser st a
line p = p <* newline

swimmingIn :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
p `swimmingIn` separator = many separator *> p `sepEndBy` (many1 separator)

one :: GenParser tok st a -> GenParser tok st [a]
one p = do { item <- p; return [item] }
