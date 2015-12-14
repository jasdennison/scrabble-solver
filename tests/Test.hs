import Test.Tasty

import qualified Board_Test as B
import qualified Parsers_Test as P
import qualified Scorer_Test as S
import qualified Trie_Test as T

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ B.tests
                          , P.tests
                          , S.tests
                          , T.tests
                          ]

