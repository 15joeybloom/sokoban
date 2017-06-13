import Lib as Sok
import Test.Hspec
import Test.QuickCheck

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
      describe "make_warehouse" $ do
        it "calculates correctly the Warehouse dimensions" $ do
          warehouse_dimensions w `shouldBe` (4, 5)
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
        it "Moves the Player into Empty Spaces" $
          let (w2, m) = move West w
           in do m `shouldBe` Walk West
                 get w2 0 0 `shouldBe` Space Player Dot
                 get w2 0 1 `shouldBe` Space Empty Not
        it "Pushes a Box when the other side is Empty Space" $
          let (w2, m) = move South w
           in do m `shouldBe` Push South
                 get w2 0 1 `shouldBe` Space Empty Not
                 get w2 1 1 `shouldBe` Space Player Not
                 get w2 2 1 `shouldBe` Space Box Not
        it "Doesn't move the Player outside of the Warehouse" $
          should_not_move North w
        it "Doesn't move the Player into a Wall" $
          let (w2, m) = move West w
           in should_not_move South w2
        it "Doesn't push a Box into a Wall" $ should_not_move East w
        it "Doesn't push a Box outside of the Warehouse" $
          let (w2, m) = move South w
           in should_not_move East w2
        it "Doesn't push more than one Box" $
          let (w2, m) = move South w
           in should_not_move South w2
  where
    Just w =
      make_warehouse
        [ [Space Empty Dot, Space Player Not, Space Box Not, Wall]
        , [Wall, Space Box Not, Space Box Not]
        , [Wall, Space Empty Not]
        , [Wall, Space Box Not]
        , [Wall, Space Empty Not]
        ]
--Idea: property test that the number of Boxes, Dots, doesn't change for a
--random move on a random board.
--Idea: property test that the board is unchanged after a NoMove is returned
