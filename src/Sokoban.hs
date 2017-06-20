{-# LANGUAGE DeriveGeneric #-}

module Sokoban
  ( Contents(..)
  , Dot(..)
  , Square(..)
  , Warehouse
  , warehouse_from_list
  , warehouse_from_map
  , warehouse_dimensions
  , get
  , Direction(..)
  , Move(..)
  , move
  , warehouse_solved
  , undo
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
  , player_row :: Int
  , player_column :: Int
  , move_stack :: [Move]
  } deriving (Show, Eq)

is_player (Space Player _) = True
is_player _ = False

warehouse_from_list :: [[Square]] -> Maybe Warehouse
warehouse_from_list grid = do
  pr <- findIndex (any is_player) grid
  pc <- findIndex is_player $ grid !! pr
  return $ WH newmap w h pr pc []
  where
    newmap = foldl aux H.empty $ zip [0 ..] grid
    aux h (r, xs) = foldl aux2 h $ zip [0 ..] xs
      where
        aux2 h (c, x) = H.insert (r, c) x h
    w = foldl (flip $ max . length) 0 grid
    h = length grid

warehouse_from_map :: H.HashMap (Int, Int) Square -> Maybe Warehouse
warehouse_from_map map
  | length players_map /= 1 = Nothing
  | otherwise = Just $ WH newmap w h pr pc []
  where
    (pr, pc) = head $ keys players_map
    players_map = H.filter is_player newmap
    w = (+ 1) $ foldl (\x (_, c) -> max x c) 0 coords
    h = (+ 1) $ foldl (\x (r, _) -> max x r) 0 coords
    coords = keys newmap
    newmap = filterWithKey (\(r, c) v -> r >= 0 && c >= 0 && v /= Wall) map

warehouse_dimensions wh = (width wh, height wh)

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

one_eighty North = South
one_eighty South = North
one_eighty West = East
one_eighty East = West

undo :: Warehouse -> Warehouse
undo warehouse@(WH {move_stack = (NoMove:moves)}) =
  warehouse {move_stack = moves}
undo warehouse@(WH {move_stack = (Walk dir:moves)}) =
  moved_back {move_stack = moves}
  where
    moved_back = fst $ move (one_eighty dir) warehouse
undo warehouse@(WH newmap w h r' c' (Push dir:moves)) = WH oldmap w h r c moves
  where
    oldmap =
      H.insert (r, c) (Space Player d) $
      H.insert (r', c') (Space Box d') $
      H.insert (r'', c'') (Space Empty d'') newmap
    Space Empty d = get warehouse r c
    Space Player d' = get warehouse r' c'
    Space Box d'' = get warehouse r'' c''
    (r, c) = step (one_eighty dir) r' c'
    (r'', c'') = step dir r' c'
undo other = other

warehouse_solved :: Warehouse -> Bool
warehouse_solved = all aux . whmap
  where
    aux (Space Box Dot) = True
    aux (Space _ Dot) = False
    aux _ = True
