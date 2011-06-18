module SSH.Config
  ( Config(..)
  , Section(..)
  , HostOption(..)
  , alias
  , hostName
  , nonIdentifyingOptions
  , label
  , user
  , port
  , parser
  )
where
import Char
import Control.Applicative hiding(many, (<|>))
import Control.Monad (ap)
import Data.List (find)
import Data.Maybe
import Text.ParserCombinators.Parsec hiding(space, label)

{- REPRESENTATION -}

data Config = Config { sections :: [Section] }
  deriving (Show)

data Section =
    HostSection { names :: [String]
                , options :: [HostOption]
                }
  deriving (Show)

type HostOption = (String, String)

optionName :: HostOption -> String
optionName = fst


alias :: Section -> String
alias = head . names

hostName :: Section -> String
hostName section = fromMaybe (alias section) $ lookup "HostName" (options section)

nonIdentifyingOptions :: Section -> [HostOption]
nonIdentifyingOptions = filter ((flip notElem) ["HostName", "Port"] . fst) . options

label :: Section -> String
label section = if friendly == detail
                then friendly
                else friendly ++ " (" ++ detail ++ ")"
  where
    friendly = alias section
    detail = userPart ++ hostName section ++ portPart
    userPart = maybe "" (++ "@") $ user section
    portPart = maybe "" (":" ++) $ port section

user :: Section -> Maybe String
user = lookup "User" . options

port :: Section -> Maybe String
port = lookup "Port" . options


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
    case kw of "HostName" -> (,) "HostName" <$> hostNameP
               _ -> (,) kw <$> many (noneOf "\n")
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
