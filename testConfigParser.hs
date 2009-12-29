import SSH.Config
import System
import Text.ParserCombinators.Parsec (parse, ParseError)

main = do
  stdin <- getContents
  handleParse $ parse parser "<stdin>" stdin

handleParse :: Either ParseError Config -> IO ()
handleParse (Left err) = print err >> exitFailure
handleParse (Right config) = print config
