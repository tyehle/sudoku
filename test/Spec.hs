import Test.Tasty
import Test.Tasty.HUnit

import ParserSpec (parserTests)
import BoardSpec (boardTests)
import StrategiesSpec (strategiesTests)

main = defaultMain tests

tests = testGroup "all tests" [parserTests, boardTests, strategiesTests]