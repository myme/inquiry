import Test.Hspec

import Inquiry.Test.Commands
import Inquiry.Test.Request
import Inquiry.Test.Zipper

main :: IO ()
main = hspec $ do
  commandsTests
  requestTests
  zipperTests
