module Main where 

import Parser
import System.IO
import System.Environment


runParser :: String -> IO ()
runParser str =
  case parseString str of
    Left err -> print err
    Right r -> print r

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parseString input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)


main :: IO ()
main = do
  args <- getArgs
  parseFromFile (args !! 1)
 -- runParser "d :- d.  d (a b) :- d.  d (a B) :- (d, c A); f." 
         --    a (b c)  .  f :- e. a :- (((((d)))))."