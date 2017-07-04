{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Monad
import Data.Maybe (fromJust)
import Graphics.Vty
import System.Environment (getArgs)
import System.IO

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
  args <- getArgs
  case args of
    [] -> startGame warehouse
    (name:_) ->
      withFile name ReadMode (\handle -> do
        contents <- hGetContents handle
        startGame $ fromJust $ warehouseFromString contents)

startGame w = do
  vty <- mkVty defaultConfig
  drawUI vty w ""
  gameLoop vty w
  shutdown vty
  putStrLn "Thanks for playing!"

gameLoop vty wh =
  if warehouseSolved wh
    then quit "Congratulations!"
    else do
      e <- nextEvent vty
      case e of
        EvKey (KChar 'u') [] -> do
          let newwh = undo wh
              m = "Undo"
          drawUI vty newwh $ show m
          gameLoop vty newwh
        EvKey (KChar 'q') [] -> quit "Bye for now."
        e -> case evDir e of
          Nothing -> gameLoop vty wh
          Just dir -> do
            let (newwh, moveDone) = move dir wh
            drawUI vty newwh $ show moveDone
            gameLoop vty newwh
  where
    quit m = do
      drawUI vty wh $ m ++ " Press q to exit."
      e <- nextEvent vty
      unless (EvKey (KChar 'q') [] == e) $ quit m
    evDir (EvKey (KChar 'h') []) = Just West
    evDir (EvKey (KChar 'j') []) = Just South
    evDir (EvKey (KChar 'k') []) = Just North
    evDir (EvKey (KChar 'l') []) = Just East
    evDir (EvKey (KChar 'w') []) = Just North
    evDir (EvKey (KChar 'a') []) = Just West
    evDir (EvKey (KChar 's') []) = Just South
    evDir (EvKey (KChar 'd') []) = Just East
    evDir (EvKey KLeft []) = Just West
    evDir (EvKey KRight []) = Just East
    evDir (EvKey KUp []) = Just North
    evDir (EvKey KDown []) = Just South
    evDir _ = Nothing
