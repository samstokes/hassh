module SSH.Config
  ( Config(..)
  , Section(..)
  , HostOption(..)
  , parser
  )
where
import Char
import Control.Applicative hiding(many)
import Control.Monad (ap)
import Text.ParserCombinators.Parsec hiding(space)

{- REPRESENTATION -}

data Config = Config { sections :: [Section] }
  deriving (Show)

data Section =
    HostSection { name :: String
                , options :: [HostOption]
                }
  deriving (Show)

data HostOption =
    HostName String
  | UnknownOption { keyword :: String, rest :: String }
  deriving (Show)


{- PARSER -}

-- nice Applicative goodness for Parsec
instance Applicative (GenParser a st) where
  (<*>) = ap
  pure = return


parser :: CharParser st Config
parser = configP

configP :: CharParser st Config
configP = Config <$> hostSectionP `swimmingIn` newline <* eof

hostSectionP :: CharParser st Section
hostSectionP = HostSection <$>
    hostHeaderP <*> many1 hostOptionP

hostHeaderP :: CharParser st String
hostHeaderP = line $ string "Host" >> space >> hostNameP

hostOptionP :: CharParser st HostOption
hostOptionP = line $ (do
    kw <- many1 letter <* space
    case kw of "HostName" -> HostName <$> hostNameP
               _ -> UnknownOption kw <$> many (noneOf "\n")
    <?> "config option")

hostNameP :: CharParser st String
hostNameP = many1 $ satisfy $ not . isSpace

-- override 'space' in Text.ParserCombinators.Parsec which matches any whitespace
space :: CharParser st Char
space = char ' '

line :: CharParser st a -> CharParser st a
line p = p <* newline

swimmingIn :: GenParser tok st a -> GenParser tok st sep -> GenParser tok st [a]
p `swimmingIn` separator = many separator *> p `sepEndBy` (many1 separator)
