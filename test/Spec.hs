import qualified Data.HashMap.Strict as H
import Data.Maybe (fromJust)
import Generic.Random.Generic
import Sokoban
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

instance Arbitrary Contents where
  arbitrary = frequency [(1, return Box), (5, return Empty)]

instance Arbitrary Dot where
  arbitrary = genericArbitrary uniform

instance Arbitrary Square where
  arbitrary =
    frequency [(10, Space <$> arbitrary <*> arbitrary), (1, return Wall)]

instance Arbitrary Direction where
  arbitrary = genericArbitrary uniform

instance Arbitrary Warehouse where
  arbitrary = do
    pr <- elements [0 .. 9]
    pc <- elements [0 .. 9]
    squares <- sequence $ repeat arbitrary
    let map = H.fromList $ zip [(x, y) | x <- [0 .. 9], y <- [0 .. 9]] squares
    return . fromJust . warehouseFromMap $
      H.insert (pr, pc) (Space Player Not) map

shouldNotMove dir w = do
  m `shouldBe` NoMove
  w2 `shouldBe` w
  where
    (w2, m) = move dir w

main :: IO ()
main =
  hspec $ do
    describe "Warehouse" $ do
      let Just w =
            warehouseFromList
              [ [Space Empty Dot, Space Player Not, Space Box Not, Wall]
              , [Wall, Space Box Not, Space Box Not]
              , [Wall, Space Empty Not]
              , [Wall, Space Box Not]
              , [Wall, Space Empty Not]
              ]
      describe "warehouseFromString" $ do
        it "works" $
          warehouseFromString "dpbw\nwbb\nw \nwb\nw \n" `shouldBe` Just w
        it "returns Nothing if there is no Player" $
          warehouseFromString "w" `shouldBe` Nothing
        it "returns Nothing if there are multiple Players" $
          warehouseFromString "pp" `shouldBe` Nothing
      describe "warehouseFromList" $ do
        it "calculates correctly the Warehouse dimensions" $
          warehouseDimensions w `shouldBe` (3, 5)
        it "returns Nothing if there is no Player" $
          warehouseFromList [[Wall]] `shouldBe` Nothing
        it "returns Nothing if there are multiple Players" $
          warehouseFromList [[Space Player Not], [Space Player Not]] `shouldBe`
          Nothing
      describe "warehouseFromMap" $ do
        it "calculates correctly the Warehouse dimensions" $ do
          let Just warehouse =
                warehouseFromMap $
                H.fromList
                  [ ((6, 6), Wall)
                  , ((3, 3), Space Box Dot)
                  , ((2, 3), Space Player Not)
                  , ((2, 4), Space Empty Not)
                  ]
          warehouseDimensions warehouse `shouldBe` (5, 4)
        it "returns Nothing if there is no Player" $ do
          warehouseFromMap H.empty `shouldBe` Nothing
          warehouseFromMap (H.singleton (1, 1) $ Space Empty Not) `shouldBe`
            Nothing
        it "returns Nothing if there are multiple Players" $ do
          let m =
                H.fromList
                  [((0, 0), Space Player Not), ((1, 1), Space Player Not)]
          warehouseFromMap m `shouldBe` Nothing
      describe "get" $ do
        it "returns Wall for coords outside the Warehouse" $ do
          get w 0 (-1) `shouldBe` Wall
          get w (-1) 0 `shouldBe` Wall
          get w 2 2 `shouldBe` Wall
          get w 0 5 `shouldBe` Wall
          get w 4 0 `shouldBe` Wall
        it "returns the correct Square for coords inside the Warehouse" $ do
          get w 0 0 `shouldBe` Space Empty Dot
          get w 0 1 `shouldBe` Space Player Not
          get w 1 0 `shouldBe` Wall
          get w 1 1 `shouldBe` Space Box Not
      describe "move" $ do
        it "Moves the Player into Empty Spaces" $ do
          let (w2, m) = move West w
          m `shouldBe` Walk West
          get w2 0 0 `shouldBe` Space Player Dot
          get w2 0 1 `shouldBe` Space Empty Not
        it "Pushes a Box when the other side is Empty Space" $ do
          let (w2, m) = move South w
          m `shouldBe` Push South
          get w2 0 1 `shouldBe` Space Empty Not
          get w2 1 1 `shouldBe` Space Player Not
          get w2 2 1 `shouldBe` Space Box Not
        it "Doesn't move the Player outside of the Warehouse" $
          shouldNotMove North w
        it "Doesn't move the Player into a Wall" $ do
          let (w2, m) = move West w
          shouldNotMove South w2
        it "Doesn't push a Box into a Wall" $ shouldNotMove East w
        it "Doesn't push a Box outside of the Warehouse" $ do
          let (w2, m) = move South w
          shouldNotMove East w2
        it "Doesn't push more than one Box" $ do
          let (w2, m) = move South w
          shouldNotMove South w2
      describe "undo" $
        prop "Returns the board to exactly the same state" propUndo

propUndo wh d = (== wh) $ undo $ fst $ move d wh
--Idea: property test that the number of Boxes, Dots, doesn't change for a
--random move on a random board.
--Idea: property test that the board is unchanged after a NoMove is returned
