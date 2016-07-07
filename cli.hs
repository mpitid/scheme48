
module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let a1 = read $ args !! 0
  let a2 = read $ args !! 1
  putStrLn $ "Hello, " ++ show (a1 + a2)

