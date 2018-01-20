import           BoardRenderer
import           GameBoard
import           Graphics.Gloss
import           Test.HUnit
import           Test.QuickCheck

coordsCheck = quickCheck ((\(x,y) -> toScreenCoords (x,y) == (x*32 - 160, 352 - (y*32)) ) :: (Float,Float)->Bool)
numberCellsTest = quickCheck ((\x -> numberCells  (RowOfCells (replicate (round x)  (FilledWith white))) == zip [0..x] (replicate (round x) (FilledWith white))) :: (Float -> Bool))


main :: IO()
main = do
  coordsCheck
  numberCellsTest
