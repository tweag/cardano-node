import System.IO (BufferMode (LineBuffering), hSetBuffering, hSetEncoding, stdout, utf8)
import qualified Test.Cardano.Conformance.ShrinkIndex
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetEncoding stdout utf8
  defaultMain $
    testGroup
      "Conformance test framework tests"
      [ Test.Cardano.Conformance.ShrinkIndex.tests
      ]
