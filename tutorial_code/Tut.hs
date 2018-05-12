#!/usr/bin/env stack
-- stack --resolver lts-11.7 script --nix


-- ghc extension to make dealing with strings easier
{-# LANGUAGE OverloadedStrings #-}


import Turtle
import Turtle.Line
import Control.Monad (liftM)
import qualified Data.Text as T
import qualified Control.Foldl as L
import Data.Maybe (isJust, fromJust)

-- example = do                        --
--     x <- select [1, 2]              -- for x in 1 2; do
--     y <- select [3, 4]              --     for y in 3 4; do
--     liftIO (print (x, y))           --         echo \(${x},${y}\);
--                                     --     done;
-- main = sh example                   -- done

-- main = ls . stdin

main :: IO ()
main = readAndSumAvg "tmp" >>= print
-- main = view $ do
--   l <- input "tmp"
--   p <- select (parse $ l)
--   return p

-- | just a trivial wrapper around ls
getAllFilesinDir :: IO ()
getAllFilesinDir = do
  dir <- pwd       -- get the current directory
  view $ ls dir    -- read all the files in it as a stream and output that strem

-- -- concatAllFiles = do
-- --   dir <- pwd
-- --   files <- ls dir

-- | This will overwrite everything in the file
writeToFile0 = do
  x <- select [1..200]
  y <- select ['a'..'z']
  output "tmp" $ case textToLine . format w $ (x, y) of
    Nothing -> ""
    Just x  -> return x

-- | This writes to the file but with newlines! And shows that we can call
-- regular haskell functions in the stream monad
writeToFile1 = do
  x <- select [1..200]
  y <- select ['a'..'z']
  liftIO (appendFile "tmp" $ show (x,y) ++ "\n")

-- Command line arguments
-- | This'll just read in arguments and spit them back out
parser :: Parser (Text, Text)
parser = (,) <$> argText "firstArg" "The First Argument"
             <*> argText "secondArg" "The Second Argument"

argDemo :: IO ()
argDemo = do (first, second) <- options "A basic demo" optIntParser
             echo $ repr first
             echo $ repr second


intParser :: Parser (Int, Int)
intParser = (,) <$> argInt "firstInt" "The First Integer"
                <*> argInt "secondInt" "The Second Integer"

optIntParser :: Parser (Int, Maybe Int)
optIntParser = (,) <$> argInt "firstInt" "The First Integer"
                   <*> optional (argInt "optional secondInt" "The Second Integer now not required")

-- cat' = stdout stdin

dumpTofile file = output file $ foldr1 (<|>) $ fmap return lines
  where
    lines :: [Line]
    lines = ((repr .) .) . (,,) <$> [1..200] <*> ['a'..'z'] <*> ['a'..'z']


sumStream :: Num a => [a] -> a
sumStream = L.fold L.sum

-- average :: (Fractional a, Num a) => [a] -> a
average :: (Fractional a) => Fold a a
average = (/) <$> L.sum <*> L.genericLength

avgStream :: (Foldable f, Fractional a) => f a -> a
avgStream = L.fold average

-- | We can define our own folds using the Fold constructor. We just need to
-- provide the step function, initial value and the extraction operation. If you
-- know anything about profunctors this may tickle your fancy
myFold f initial = Fold (\acc (e,_,_) -> e `f` acc) initial id

sumAvg :: Fold (Double, t1, t) (Double, Double)
sumAvg = L.premap fst' ((,) <$> L.sum <*> average)
  where fst' (x,_,_) = x

readAndSumAvg :: MonadIO io => Turtle.FilePath -> io (Double, Double)
-- readAndSumAvg file = fold (input file) consume
readAndSumAvg = flip fold consume . input

parse :: Line -> (Double, Char, Char)
parse = head . ourPattern . lineToText

consume :: Fold Line (Double, Double)
consume = L.premap parse sumAvg

ourPattern :: Text -> [(Double, Char, Char)]
ourPattern = match $ do "("
                        first <- decimal
                        junk
                        second <- anyChar
                        junk
                        third <- anyChar
                        junk
                        ")"
                        return (fromIntegral first, second, third) -- fromIntegral to coerce to double
  where junk = star $ char ',' <|> char '\''
