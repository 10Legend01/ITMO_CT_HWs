module Main where

import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline (InputT, runInputT, getInputLine, defaultSettings, outputStrLn)

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          case parse input of
            Left pe -> outputStrLn ("Parse error: " ++ show pe)
            Right expr -> case eval expr of
                Left ee -> outputStrLn ("Eval error: " ++ show ee)
                Right v -> outputStrLn $ show $ prettyValue v
          loop
