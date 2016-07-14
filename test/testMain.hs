
import qualified Eq
import qualified Iso
import Test.QuickCheck.Simple (defaultMain)

main :: IO ()
main = defaultMain $ Eq.tests ++ Iso.tests
