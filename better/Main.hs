{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Graphics.Vty

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

warehouseImage :: Warehouse -> Image
warehouseImage wh = vertCat $ map rowImage [ 0 .. h - 1 ]
  where
    rowImage r = horizCat $ map (cellImage r) [ 0 .. w - 1 ]
    cellImage r c = string defAttr $ squareStr $ get wh r c
    (w, h) = warehouseDimensions wh
    squareStr (Space Empty Not) = "  "
    squareStr (Space Empty Dot) = "⬤ "
    squareStr (Space Box Not) = "⬛ "
    squareStr (Space Box Dot) = "⬜ "
    squareStr (Space Player Not) = "☆ "
    squareStr (Space Player Dot) = "★ "
    squareStr Wall = "▦ "

main = do
  vty <- mkVty defaultConfig
  update vty $ picForImage $ warehouseImage warehouse
  gameLoop vty warehouse
  shutdown vty
  putStrLn "Thanks for playing!"

gameLoop vty wh =
  if warehouseSolved wh
    then quit "Congratulations! Press any key to exit."
    else do
      e <- nextEvent vty
      case e of
        EvKey (KChar 'u') [] -> do
          let newwh = undo wh
              m = "Undo"
          recurse newwh m
        EvKey (KChar 'q') [] -> quit "Bye for now."
        EvKey (KChar input) [] -> do
          let (newwh, moveDone) =
                case charDir input of
                  Just dir -> move dir wh
                  Nothing -> (wh, NoMove)
          recurse newwh $ show moveDone
        _ -> gameLoop vty wh
  where
    recurse newwh m = do
      update vty $ picForImage $ warehouseImage newwh <-> string defAttr m
      gameLoop vty newwh
    quit m = do
      update vty $
        picForImage $
        warehouseImage wh <-> string defAttr (m ++ " Press q to exit.")
      e <- nextEvent vty
      unless (EvKey (KChar 'q') [] == e) $ quit m
    charDir 'h' = Just West
    charDir 'j' = Just South
    charDir 'k' = Just North
    charDir 'l' = Just East
    charDir 'w' = Just North
    charDir 'a' = Just West
    charDir 's' = Just South
    charDir 'd' = Just East
    charDir _ = Nothing
