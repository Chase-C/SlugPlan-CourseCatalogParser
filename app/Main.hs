module Main where

import System.IO
import UCSCScraper
import Subjects
import PrereqParser

main :: IO ()
--main = do
--    text <- parsePdf "2015-16-catalog.pdf"
--    writeFile "out3.txt" text
main = parseText "out3.txt"
