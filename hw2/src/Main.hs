module Main where

import           Block1 (testProp12, testUnit11)
import           Block2 (testProp21, testProp23, testUnit21)

main :: IO ()
main = putStrLn ("\n--------------- hw2 ---------------"  ++
                 "\nUse imported functions for testing.\n" ++
                 "\nBlock1 | Task1 : testUnit11" ++
                 "\n       | Task2 : testProp12\n" ++
                 "\nBlock2 | Task1 : testUnit21, testProp21" ++
                 "\n       | Task2" ++
                 "\n       | Task3 : testProp23\n" ++
                 "\nBlock3 | Task1" ++
                 "\n       | Task2 : testUnit32" ++
                 "\n       | Task3 : testUnit33" ++
                 "\n       | Task3 : testUnit34\n")
