import SSH.Config
import System
import Text.ParserCombinators.Parsec (parse, ParseError)

main :: IO ()
main = do
  stdin <- getContents
  handleParse $ parse parser "<stdin>" stdin

handleParse :: Either ParseError Config -> IO ()
handleParse (Left err) = print err >> exitFailure
handleParse (Right config) = print $ oneCharNames (sections config)

oneCharNames :: [Section] -> [Char]
oneCharNames sects = [nm !! 0 | HostSection nm _ <- sects, length nm == 1]
