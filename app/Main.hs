module Main where

import Sokoban

Just warehouse =
  warehouseFromList
    [ [Wall, Wall, Wall, Wall, Wall, Wall]
    , [Wall, mpty, mpty, mpty, mpty, Wall]
    , [Wall, mpty, boxn, boxd, mpty, Wall]
    , [Wall, mpty, plyr, mptd, mpty, Wall]
    , [Wall, mpty, mpty, mpty, mpty, Wall]
    , [Wall, Wall, Wall, Wall, Wall, Wall]
    ]
  where
    mpty = Space Empty Not
    mptd = Space Empty Dot
    plyr = Space Player Not
    boxn = Space Box Not
    boxd = Space Box Dot

main :: IO ()
main = do
  putStrLn "Welcome to Sokoban!"
  gameLoop warehouse

gameLoop :: Warehouse -> IO ()
gameLoop wh = do
  putStr "Moves: "
  print $ moveCount wh
  printWarehouse wh
  if warehouseSolved wh
    then do
      putStrLn "Congratulations!"
      return ()
    else do
      putStrLn "hjkl or wasd to move: "
      input <- getChar
      putStrLn ""
      case input of
        'u' -> do
          putStrLn "Undo"
          gameLoop $ undo wh
        _ -> do
          let (newwh, moveDone) =
                case charDir input of
                  Just dir -> move dir wh
                  Nothing -> (wh, NoMove)
          print moveDone
          gameLoop newwh
  where
    charDir 'h' = Just West
    charDir 'j' = Just South
    charDir 'k' = Just North
    charDir 'l' = Just East
    charDir 'w' = Just North
    charDir 'a' = Just West
    charDir 's' = Just South
    charDir 'd' = Just East
    charDir _ = Nothing

printWarehouse :: Warehouse -> IO ()
printWarehouse wh = mapM_ printRow [0 .. w - 1]
  where
    printRow r = do
      mapM_ (printCell r) [0 .. h - 1]
      putChar '\n'
    printCell r c = putStr $ squareStr $ get wh r c
    (w, h) = warehouseDimensions wh
    squareStr (Space Empty Not) = "  "
    squareStr (Space Empty Dot) = "⬤ "
    squareStr (Space Box Not) = "⬛ "
    squareStr (Space Box Dot) = "⬜ "
    squareStr (Space Player Not) = "☆ "
    squareStr (Space Player Dot) = "★ "
    squareStr Wall = "▦ "
