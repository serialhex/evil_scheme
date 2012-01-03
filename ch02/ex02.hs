module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value! YAY!!!"

spaces :: Parser ()
spaces = skipMany1 space

-- lispval-thing haskell-thing
data LispVal  = Atom String
              | List [LispVal]
              | DottedList [LispVal] LispVal
              | Number Integer
              | String String
              | Bool Bool

parseString :: Parser LispVal
parseString  = do char '"'
                  x <- many (noneOf "\"")
                  char '"'
                  return $ String x

parseAtom :: Parser LispVal
parseAtom  = do first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                            "#t" -> Bool True
                            "#f" -> Bool False
                            _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))
