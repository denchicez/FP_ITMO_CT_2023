module Main (main) where
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)
import HW5.Action
import HW5.Evaluator (eval)
import Data.Set (fromList)
import Control.Monad.IO.Class (MonadIO (liftIO))

main :: IO ()
main = runInputT defaultSettings loop
 where
   loop = do
     line <- getInputLine "hi> "
     case line of
       Just inputSuccess -> case parse $ inputSuccess of
         Right parseSuccess -> do
           evalValue <- liftIO (runHIO (eval parseSuccess) (fromList [AllowTime, AllowRead, AllowWrite]))
           case evalValue of
             Right evalValueSuccess -> do (outputStrLn $ (show (prettyValue evalValueSuccess)))
             _ -> do (outputStrLn "can't eval parsing line :(((")
         Left _ -> do (outputStrLn "Can't parse line :(")
       _ -> do (outputStrLn "Can't read from console :0")
     loop
