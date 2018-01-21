import           BoardRenderer
import           GameBoard
import           Graphics.Gloss
import           Test.HUnit
import           Test.QuickCheck

coordsCheck = quickCheck ((\(x,y) -> toScreenCoords (x,y) == (x*32 - 160, 352 - (y*32)) ) :: (Float,Float)->Bool)
numberCellsTest = quickCheck ((\x -> let n = round $ abs x in numberCells  (RowOfCells (replicate n  Empty)) == zip [0..9] (replicate n Empty)) :: (Float -> Bool))

test1 = TestCase (assertEqual "1" 1)

main :: IO()
main = do
  coordsCheck
  numberCellsTest
  test1
