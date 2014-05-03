import           Test.Tasty (defaultMain)

import qualified PushTests

main :: IO ()
main = defaultMain PushTests.tests
