import           Test.Framework (defaultMain)

import qualified PushTests

main :: IO ()
main = defaultMain
    [ PushTests.tests
    ]
