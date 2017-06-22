module Main where

import Sokoban

Just warehouse =
  warehouse_from_list
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
  game_loop warehouse

game_loop :: Warehouse -> IO ()
game_loop wh = do
  putStr "Moves: "
  putStrLn . show $ move_count wh
  print_warehouse wh
  if warehouse_solved wh
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
          game_loop $ undo wh
        _ -> do
          let (newwh, move_done) =
                case char_dir input of
                  Just dir -> move dir wh
                  Nothing -> (wh, NoMove)
          putStrLn $ show move_done
          game_loop newwh
  where
    char_dir 'h' = Just West
    char_dir 'j' = Just South
    char_dir 'k' = Just North
    char_dir 'l' = Just East
    char_dir 'w' = Just North
    char_dir 'a' = Just West
    char_dir 's' = Just South
    char_dir 'd' = Just East
    char_dir _ = Nothing

print_warehouse :: Warehouse -> IO ()
print_warehouse wh = sequence_ $ map print_row [0 .. w - 1]
  where
    print_row r = do
      sequence_ $ map (print_cell r) [0 .. h - 1]
      putChar '\n'
    print_cell r c = putStr $ square_str $ get wh r c
    (w, h) = warehouse_dimensions wh
    square_str (Space Empty Not) = "  "
    square_str (Space Empty Dot) = "⬤ "
    square_str (Space Box Not) = "⬛ "
    square_str (Space Box Dot) = "⬜ "
    square_str (Space Player Not) = "☆ "
    square_str (Space Player Dot) = "★ "
    square_str Wall = "▦ "
