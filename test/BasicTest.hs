module Main where

import Control.Monad (void)
import Test.HUnit
import HaskYang (Yang( .. ))

main :: IO ()
main = void <$> runTestTT $ TestList tests

tests =
    [ TestCase (assertEqual "1 == 1" 1 1)
    ]
