{-# LANGUAGE DeriveGeneric #-}

module Sokoban
  ( Contents(..)
  , Dot(..)
  , Square(..)
  , Warehouse
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

import Data.HashMap.Strict as H
import Data.List (findIndex)
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
  , playerRow :: Int
  , playerColumn :: Int
  , moveStack :: [Move]
  } deriving (Show, Eq)

isPlayer (Space Player _) = True
isPlayer _ = False

warehouseFromList :: [[Square]] -> Maybe Warehouse
warehouseFromList grid = do
  pr <- findIndex (any isPlayer) grid
  pc <- findIndex isPlayer $ grid !! pr
  return $ WH newmap w h pr pc []
  where
    newmap = foldl aux H.empty $ zip [0 ..] grid
    aux h (r, xs) = foldl aux2 h $ zip [0 ..] xs
      where
        aux2 h (c, x) = H.insert (r, c) x h
    w = foldl (flip $ max . length) 0 grid
    h = length grid

warehouseFromMap :: H.HashMap (Int, Int) Square -> Maybe Warehouse
warehouseFromMap map
  | length playersMap /= 1 = Nothing
  | otherwise = Just $ WH newmap w h pr pc []
  where
    (pr, pc) = head $ keys playersMap
    playersMap = H.filter isPlayer newmap
    w = (+ 1) $ foldl (\x (_, c) -> max x c) 0 coords
    h = (+ 1) $ foldl (\x (r, _) -> max x r) 0 coords
    coords = keys newmap
    newmap = filterWithKey (\(r, c) v -> r >= 0 && c >= 0 && v /= Wall) map

warehouseDimensions wh = (width wh, height wh)

get :: Warehouse -> Int -> Int -> Square
get warehouse r c = H.lookupDefault Wall (r, c) $ whmap warehouse

step North r c = (r - 1, c)
step South r c = (r + 1, c)
step West r c = (r, c - 1)
step East r c = (r, c + 1)

move :: Direction -> Warehouse -> (Warehouse, Move)
move dir warehouse@(WH oldmap w h r c stk) =
  case get warehouse r' c' of
    Space Empty d' ->
      let newmap =
            (H.insert (r, c) (Space Empty d) $
             H.insert (r', c') (Space Player d') oldmap)
          move = Walk dir
       in (WH newmap w h r' c' (move : stk), move)
    Space Box d' ->
      case get warehouse r'' c'' of
        Space Empty d'' ->
          let newmap =
                H.insert (r, c) (Space Empty d) $
                H.insert (r', c') (Space Player d') $
                H.insert (r'', c'') (Space Box d'') oldmap
              move = Push dir
           in (WH newmap w h r' c' (move : stk), move)
        _ -> (warehouse, NoMove)
    _ -> (warehouse, NoMove)
  where
    Space Player d = get warehouse r c
    (r', c') = step dir r c
    (r'', c'') = step dir r' c'

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
undo warehouse@(WH newmap w h r' c' (Push dir:moves)) = WH oldmap w h r c moves
  where
    oldmap =
      H.insert (r, c) (Space Player d) $
      H.insert (r', c') (Space Box d') $
      H.insert (r'', c'') (Space Empty d'') newmap
    Space Empty d = get warehouse r c
    Space Player d' = get warehouse r' c'
    Space Box d'' = get warehouse r'' c''
    (r, c) = step (oneEighty dir) r' c'
    (r'', c'') = step dir r' c'
undo other = other

moveCount = length . moveStack

warehouseSolved :: Warehouse -> Bool
warehouseSolved = all aux . whmap
  where
    aux (Space Box Dot) = True
    aux (Space _ Dot) = False
    aux _ = True
