import qualified Data.HashMap.Strict as H
import Data.Maybe (fromMaybe)
import Generic.Random.Generic
import Sokoban
import Test.Hspec
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
    pr <- elements [0..9]
    pc <- elements [0..9]
    squares <- sequence $ repeat $ arbitrary
    let map = H.fromList $ zip [(x, y) | x <- [0 .. 9], y <- [0 .. 9]] squares
    return $
      fromMaybe (error "shit") $
        warehouse_from_map $ H.insert (pr, pc) (Space Player Not) map

should_not_move :: Direction -> Warehouse -> Expectation
should_not_move dir w = do
  m `shouldBe` NoMove
  w2 `shouldBe` w
  where
    (w2, m) = move dir w

main :: IO ()
main =
  hspec $ do
    describe "Warehouse" $ do
      let Just w =
            warehouse_from_list
              [ [Space Empty Dot, Space Player Not, Space Box Not, Wall]
              , [Wall, Space Box Not, Space Box Not]
              , [Wall, Space Empty Not]
              , [Wall, Space Box Not]
              , [Wall, Space Empty Not]
              ]
      describe "warehouse_from_list" $ do
        it "calculates correctly the Warehouse dimensions" $ do
          warehouse_dimensions w `shouldBe` (4, 5)
        it "returns Nothing if there is no Player" $ do
          warehouse_from_list [[Wall]] `shouldBe` Nothing
      describe "warehouse_from_map" $ do
        it "returns Nothing if there is no Player" $ do
          warehouse_from_map H.empty `shouldBe` Nothing
          warehouse_from_map (H.singleton (1, 1) $ Space Empty Not) `shouldBe`
            Nothing
        it "returns Nothing if there is more than one Player" $ do
          let m =
                H.fromList
                  [((0, 0), Space Player Not), ((1, 1), Space Player Not)]
          warehouse_from_map m `shouldBe` Nothing
        it "calculates correctly the Warehouse dimensions" $ do
          let Just warehouse =
                warehouse_from_map $
                H.fromList
                  [ ((6, 6), Wall)
                  , ((3, 3), Space Box Dot)
                  , ((2, 3), Space Player Not)
                  , ((2, 4), Space Empty Not)
                  ]
          warehouse_dimensions warehouse `shouldBe` (5, 4)
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
          should_not_move North w
        it "Doesn't move the Player into a Wall" $ do
          let (w2, m) = move West w
          should_not_move South w2
        it "Doesn't push a Box into a Wall" $ should_not_move East w
        it "Doesn't push a Box outside of the Warehouse" $ do
          let (w2, m) = move South w
          should_not_move East w2
        it "Doesn't push more than one Box" $ do
          let (w2, m) = move South w
          should_not_move South w2
      describe "undo" $ do
        it "Returns the board to exactly the same state" $ do
          property prop_undo

prop_undo wh d = (== wh) $ undo $ fst $ move d wh
--Idea: property test that the number of Boxes, Dots, doesn't change for a
--random move on a random board.
--Idea: property test that the board is unchanged after a NoMove is returned