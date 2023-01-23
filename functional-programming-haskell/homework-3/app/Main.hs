module Main (
    main,
)
where

import Control.Monad.IO.Class
import Data.Set
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "hi> "
        case minput of
            Nothing -> return ()
            Just ":q" -> return ()
            Just "" -> loop
            Just input -> do
                printer <- getExternalPrint
                let parseResult = parse input
                case parseResult of
                    Left parseError -> liftIO $ printer $ show parseError
                    Right parsed -> do
                        let evalResult = eval parsed
                        let allPermissions = fromList [AllowRead, AllowWrite, AllowTime]
                        r <- liftIO $ runHIO evalResult allPermissions
                        liftIO $ case r of
                            Left evalError -> printer $ show evalError
                            Right result -> printer $ show $ prettyValue result
                loop
