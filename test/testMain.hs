
import qualified Eq
import qualified Iso
import Test.QuickCheck.Simple (defaultMain')

main :: IO ()
main = defaultMain' True $ Eq.tests ++ Iso.tests
