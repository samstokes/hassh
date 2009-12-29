import SSH.Config
import Text.ParserCombinators.Parsec (runParser, ParseError)

main = do
  stdin <- getContents
  handleParse $ runParser parser () "<stdin>" stdin

handleParse :: Either ParseError Config -> IO ()
handleParse (Left err) = print err
handleParse (Right config) = print config
