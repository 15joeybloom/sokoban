{-# LANGUAGE DeriveGeneric #-}

module Sokoban
  ( Contents(..)
  , Dot(..)
  , Square(..)
  , Warehouse
  , warehouseFromString
  , warehouseFromList
  , warehouseFromMap
  , warehouseDimensions
  , get
  , Direction(..)
  , Move(..)
  , move
  , warehouseSolved
  , undo
  , moveCount
  ) where

import Control.Arrow ((&&&))
import qualified Data.HashMap.Strict as H
import GHC.Generics (Generic)

data Contents
  = Player
  | Box
  | Empty
  deriving (Show, Eq, Generic)

data Dot
  = Dot
  | Not
  deriving (Show, Eq, Generic)

data Square
  = Space Contents
          Dot
  | Wall
  deriving (Show, Eq, Generic)

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq, Generic)

data Move
  = Walk Direction
  | Push Direction
  | NoMove
  deriving (Show, Eq, Generic)

data Warehouse = WH
  { whmap :: H.HashMap (Int, Int) Square
  , width :: Int
  , height :: Int
  , _playerRow :: Int
  , _playerColumn :: Int
  , moveStack :: [Move]
  } deriving (Show, Eq)

isPlayer :: Square -> Bool
isPlayer (Space Player _) = True
isPlayer _ = False

warehouseFromString :: String -> Maybe Warehouse
warehouseFromString = warehouseFromList . (map . map) charSquare . lines
  where
    charSquare 'w' = Wall
    charSquare 'b' = Space Box Not
    charSquare 'c' = Space Box Dot
    charSquare 'p' = Space Player Not
    charSquare 'q' = Space Player Dot
    charSquare 'd' = Space Empty Dot
    charSquare _   = Space Empty Not

warehouseFromList :: [[Square]] -> Maybe Warehouse
warehouseFromList grid = warehouseFromMap newMap
  where
    newMap = foldl aux H.empty $ zip [0 ..] grid
    aux mapWithoutRow (r, xs) = foldl aux2 mapWithoutRow $ zip [0 ..] xs
      where
        aux2 mapWithoutCell (c, x) = H.insert (r, c) x mapWithoutCell

warehouseFromMap :: H.HashMap (Int, Int) Square -> Maybe Warehouse
warehouseFromMap oldMap
  | length playersMap /= 1 = Nothing
  | otherwise = Just $ WH newMap w h pr pc []
  where
    (pr, pc) = head $ H.keys playersMap
    playersMap = H.filter isPlayer newMap
    w = (+ 1) $ foldl (\x (_, c) -> max x c) 0 coords
    h = (+ 1) $ foldl (\x (r, _) -> max x r) 0 coords
    coords = H.keys newMap
    newMap = H.filterWithKey (\(r, c) _ -> r >= 0 && c >= 0) oldMap

warehouseDimensions :: Warehouse -> (Int, Int)
warehouseDimensions = width &&& height

get :: Warehouse -> Int -> Int -> Square
get warehouse r c = H.lookupDefault Wall (r, c) $ whmap warehouse

step :: (Num r, Num c) => Direction -> r -> c -> (r, c)
step North r c = (r - 1, c)
step South r c = (r + 1, c)
step West r c = (r, c - 1)
step East r c = (r, c + 1)

move :: Direction -> Warehouse -> (Warehouse, Move)
move dir warehouse@(WH oldMap w h r c stk) =
  case get warehouse r' c' of
    Space Empty d' ->
      let newMap =
            (H.insert (r, c) (Space Empty d) $
             H.insert (r', c') (Space Player d') oldMap)
          theMove = Walk dir
       in (WH newMap w h r' c' (theMove : stk), theMove)
    Space Box d' ->
      case get warehouse r'' c'' of
        Space Empty d'' ->
          let newMap =
                H.insert (r, c) (Space Empty d) $
                H.insert (r', c') (Space Player d') $
                H.insert (r'', c'') (Space Box d'') oldMap
              theMove = Push dir
           in (WH newMap w h r' c' (theMove : stk), theMove)
        _ -> (warehouse, NoMove)
    _ -> (warehouse, NoMove)
  where
    Space Player d = get warehouse r c
    (r', c') = step dir r c
    (r'', c'') = step dir r' c'

oneEighty :: Direction -> Direction
oneEighty North = South
oneEighty South = North
oneEighty West = East
oneEighty East = West

undo :: Warehouse -> Warehouse
undo warehouse@WH {moveStack = (NoMove:moves)} =
  warehouse {moveStack = moves}
undo warehouse@WH {moveStack = (Walk dir:moves)} =
  movedBack {moveStack = moves}
  where
    movedBack = fst $ move (oneEighty dir) warehouse
undo warehouse@(WH newMap w h r' c' (Push dir:moves)) = WH oldMap w h r c moves
  where
    oldMap =
      H.insert (r, c) (Space Player d) $
      H.insert (r', c') (Space Box d') $
      H.insert (r'', c'') (Space Empty d'') newMap
    Space Empty d = get warehouse r c
    Space Player d' = get warehouse r' c'
    Space Box d'' = get warehouse r'' c''
    (r, c) = step (oneEighty dir) r' c'
    (r'', c'') = step dir r' c'
undo other = other

moveCount :: Warehouse -> Int
moveCount = length . moveStack

warehouseSolved :: Warehouse -> Bool
warehouseSolved = all aux . whmap
  where
    aux (Space Box Dot) = True
    aux (Space _ Dot) = False
    aux _ = True
