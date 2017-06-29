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
warehouseImage wh = vertCat $ map rowImage [0 .. h - 1]
  where
    (w, h) = warehouseDimensions wh
    rowImage r = horizCat $ map (cellImage r) [0 .. w - 1]
    cellImage r c =
      case get wh r c of
        (Space Empty Not) -> string myDef "  "
        (Space Empty Dot) -> string myDef "⬤ "
        (Space Box Not) -> string myDef "[]"
        (Space Box Dot) -> blackSpace <|> blackSpace
        (Space Player Not) -> string (myDef `withStyle` bold) "☆ "
        (Space Player Dot) -> string myDef "★ "
        Wall -> string (defAttr `withForeColor` grey `withBackColor` grey) "  "
    whiteSpace = string (myDef `withForeColor` white `withBackColor` white) " "
    blackSpace = string (myDef `withForeColor` black `withBackColor` black) " "
    grey = rgbColor 48 48 48

myDef = defAttr `withForeColor` black `withBackColor` white

drawUI vty wh message = update vty picture
  where
    picture =
      Picture
      {picCursor = NoCursor, picLayers = [image], picBackground = background}
    image = warehouseImage wh <-> string myDef moves <-> string myDef message
    background = Background {backgroundChar = ' ', backgroundAttr = myDef}
    moves = "Moves: " ++ show (moveCount wh)

main = do
  vty <- mkVty defaultConfig
  drawUI vty warehouse ""
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
          drawUI vty newwh $ show m
          gameLoop vty newwh
        EvKey (KChar 'q') [] -> quit "Bye for now."
        EvKey (KChar input) [] -> do
          let (newwh, moveDone) =
                case charDir input of
                  Just dir -> move dir wh
                  Nothing -> (wh, NoMove)
          drawUI vty newwh $ show moveDone
          gameLoop vty newwh
        _ -> gameLoop vty wh
  where
    quit m = do
      drawUI vty wh $ m ++ " Press q to exit."
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
