module Main (main) where
import HW6.T3 (Config (..), simulate, Comonad19Grid, Cell (..))
import Data.Grid (Grid (..))
import Data.ListZipper (ListZipper (..))
import Options.Applicative
import Data.List (intercalate)

main :: IO ()
main = startGame =<< execParser opts
  where
    opts = info (pConfig <**> helper)
      ( fullDesc
     <> progDesc "Play game of comanad19" )

prettyPrinter :: Int -> Comonad19Grid -> String
prettyPrinter sz grid = prettyListListZipper sz (unGrid grid)

prettyJustList :: [a] -> (a -> String) -> String -> String
prettyJustList lst printer delimetr = intercalate delimetr (fmap printer lst)

prettyListListZipper :: Int -> ListZipper (ListZipper Cell) -> String
prettyListListZipper sz (LZ left now right) = do
  let leftArray = reverse (take sz left)
  let rightArray = take sz right
  let prettyLeft = prettyJustList leftArray (prettyListZipper sz) "\n"
  let prettyRight = prettyJustList rightArray (prettyListZipper sz) "\n"
  intercalate "\n" [prettyLeft, prettyListZipper sz now, prettyRight]

prettyListZipper :: Int -> ListZipper Cell -> String
prettyListZipper sz (LZ left now right) = do
  let leftArray = reverse (take sz left)
  let rightArray = take sz right
  prettyJustList leftArray show "" ++ show now ++ prettyJustList rightArray show ""


startGame :: Config -> IO ()
startGame conf = do
  let answer = simulate conf
  putStrLn $ intercalate "\n\n" (fmap (prettyPrinter (gridSize conf)) (take (iterations conf) answer))

pConfig :: Parser Config
pConfig = Config
      <$> option auto
          ( long "prob"
         <> metavar "Double" )
      <*> option auto
          ( long "incub"
         <> metavar "INT" )
      <*> option auto
          ( long "ill"
         <> metavar "INT" )
      <*> option auto
          ( long "immun"
         <> metavar "INT" )
      <*> option auto
          ( long "grid-size"
         <> metavar "INT" )
      <*> option auto
          ( long "iterations"
         <> metavar "INT" )
      <*> option auto
          ( long "seed"
         <> showDefault
         <> value 1337
         <> metavar "INT" )
