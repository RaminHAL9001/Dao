import qualified Dao.Test.Interval
-- import qualified Dao.Test.Grammar

import           Control.Monad
import           System.Exit

main :: IO ()
main = (>>= (flip unless exitFailure)) $ fmap and $ sequence $
  [ 
  -- , Dao.Test.Interval.runTests
  -- , Dao.Test.Grammar.runTests
    Dao.Test.Grammar.runTests
  ]

