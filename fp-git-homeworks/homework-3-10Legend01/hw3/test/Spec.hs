import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Test.Hspec (Expectation, Spec, it, shouldReturn)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (testSpec)

main :: IO ()
main = do
  task1 <- group
  defaultMain $ testGroup "Tests" [task1]

evaluate :: String -> IO (Maybe String)
evaluate input =
  case parse input of
    Left _ -> return Nothing
    Right expr -> return $ Just $ case eval expr of
      Left e -> show e
      Right v -> show $ prettyValue v

evaluatesTo :: String -> String -> Expectation
evaluatesTo input output = do
  evaluate input `shouldReturn` Just output

group :: IO TestTree
group = testSpec "Test" spec

spec :: Spec
spec = do
  it "Tests from homework" $ do
    "2" `evaluatesTo` "2"
    "3.14" `evaluatesTo` "3.14"
    "-1.618" `evaluatesTo` "-1.618"
    "1.2e5" `evaluatesTo` "120000"
    "div(add(10, 15.1), 3)" `evaluatesTo` "8 + 11/30"
    "add(500, 12)" `evaluatesTo` "512"
    "sub(10, 100)" `evaluatesTo` "-90"
    "mul(23, 768)" `evaluatesTo` "17664"
    "div(57, 190)" `evaluatesTo` "0.3"
    "div(add(mul(2, 5), 1), sub(11,6))" `evaluatesTo` "2.2"
    "sub(1)" `evaluatesTo` "HiErrorArityMismatch"
    "sub(1, 2, 3)" `evaluatesTo` "HiErrorArityMismatch"
    "div(1, 0)" `evaluatesTo` "HiErrorDivideByZero"
    "div(1, sub(5, 5))" `evaluatesTo` "HiErrorDivideByZero"
    "15(2)" `evaluatesTo` "HiErrorInvalidFunction"
    "sub(10, add)" `evaluatesTo` "HiErrorInvalidArgument"
    "100" `evaluatesTo` "100"
    "-15" `evaluatesTo` "-15"
    "add(100, -15)" `evaluatesTo` "85"
    "div(10, 3)" `evaluatesTo` "3 + 1/3"
    "sub(mul(201, 11), 0.33)" `evaluatesTo` "2210.67"
