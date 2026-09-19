import Test.Tasty (defaultMain, testGroup)
import qualified Axiom.ParserTest

main :: IO ()
main = defaultMain $ testGroup "Axiom"
    [ Axiom.ParserTest.tests
    ]
