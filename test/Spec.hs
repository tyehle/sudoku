import Test.Tasty
import Test.Tasty.HUnit

import ParserSpec (parserTests)
import BoardSpec (boardTests)

main = defaultMain tests

tests = testGroup "all tests" [parserTests, boardTests]