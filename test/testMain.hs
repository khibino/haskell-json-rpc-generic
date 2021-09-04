
import qualified Eq
import qualified Iso
import Test.QuickCheck.Simple (defaultMain_)

main :: IO ()
main = defaultMain_ True $ Eq.tests ++ Iso.tests
