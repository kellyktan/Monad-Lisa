module Main where

import ImageParser

import Evaluator

import ParserCombinators

import ImageTransformation

--------------------------------------------------------------------------------------------------

main :: IO ()
main = go where
  go = do
    putStrLn "Please enter name of kd file to process: "
    str <- getLine
    result <- parseFromFile itP str
    case result of
      (Left _) -> do
                    putStrLn "No Parse: malformed kd file. Try again"
      (Right it) -> do
                      putStrLn "Great! What is the name of the output file?"
                      outputFile <- getLine
                      write outputFile (evaluateIT it)

-- Code to benchmark
--    result <- parseFromFile itP "kd/test.kd"
--    case result of
--      (Left _ ) -> putStrLn "no parse"
--      (Right it) -> write "temp.jpg" (evaluateIT it)
--
