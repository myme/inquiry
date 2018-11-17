import Test.Hspec

import Inquiry.Test.Commands
import Inquiry.Test.Zipper

main :: IO ()
main = hspec $ do
  commandsTests
  zipperTests
