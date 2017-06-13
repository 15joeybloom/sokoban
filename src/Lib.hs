module Lib
  ( Contents(..)
  , Dot(..)
  , Square(..)
  , Warehouse
  , make_warehouse
  , warehouse_dimensions
  , get
  , Direction(..)
  , Move(..)
  , move
  , warehouse_solved
  ) where

import Data.HashMap.Strict as H
import Data.List

data Contents
  = Player
  | Box
  | Empty
  deriving (Show, Eq)

data Dot
  = Dot
  | Not
  deriving (Show, Eq)

data Square
  = Space Contents
          Dot
  | Wall
  deriving (Show, Eq)

data Warehouse = WH
  { map :: H.HashMap (Int, Int) Square
  , width :: Int
  , height :: Int
  , player_row :: Int
  , player_column :: Int
  } deriving (Show, Eq)

is_player (Space Player _) = True
is_player _ = False

make_warehouse :: [[Square]] -> Maybe Warehouse
make_warehouse grid = do
  pr <- findIndex (any is_player) grid
  pc <- findIndex is_player $ grid !! pr
  return $ WH map w h pr pc
  where
    map = foldl aux H.empty $ zip [0 ..] grid
    aux h (r, xs) = foldl aux2 h $ zip [0 ..] xs
      where
        aux2 h (c, x) = H.insert (r, c) x h
    w = foldl (flip $ max . length) 0 grid
    h = length grid

warehouse_dimensions wh = (width wh, height wh)

get :: Warehouse -> Int -> Int -> Square
get (WH map _ _ _ _) r c = H.lookupDefault Wall (r, c) map

data Direction
  = North
  | South
  | East
  | West
  deriving (Show, Eq)

data Move
  = Walk Direction
  | Push Direction
  | NoMove
  deriving (Show, Eq)

step North r c = (r - 1, c)
step South r c = (r + 1, c)
step West r c = (r, c - 1)
step East r c = (r, c + 1)

move :: Direction -> Warehouse -> (Warehouse, Move)
move dir warehouse@(WH map w h r c) =
  case get warehouse r' c' of
    Space Empty d' ->
      let newmap =
            (H.insert (r, c) (Space Empty d) $
             H.insert (r', c') (Space Player d') map)
       in (WH newmap w h r' c', Walk dir)
    Space Box d' ->
      case get warehouse r'' c'' of
        Space Empty d'' ->
          let newmap =
                (H.insert (r, c) (Space Empty d) $
                 H.insert (r', c') (Space Player d') $
                 H.insert (r'', c'') (Space Box d'') map)
           in (WH newmap w h r' c', Push dir)
        _ -> (warehouse, NoMove)
    _ -> (warehouse, NoMove)
  where
    Space Player d = get warehouse r c
    (r', c') = step dir r c
    (r'', c'') = step dir r' c'

warehouse_solved :: Warehouse -> Bool
warehouse_solved (WH map _ _ _ _) = all aux map
  where
    aux (Space Box Dot) = True
    aux (Space _ Dot) = False
    aux _ = True
