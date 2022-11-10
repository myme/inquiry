import Inquiry.Test.Commands
import Inquiry.Test.Zipper
import Test.Hspec

main :: IO ()
main = hspec $ do
  commandsTests
  zipperTests
