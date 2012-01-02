module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Hihi, what is your name? "
  name <- getLine
  putStrLn ("Hihi " ++ name)