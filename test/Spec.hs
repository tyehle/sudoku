import Test.Tasty
import Test.Tasty.HUnit

import ParserSpec (parserTests)

main = defaultMain tests

tests = testGroup "all tests" [parserTests]