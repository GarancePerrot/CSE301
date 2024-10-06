-- setting tplayerhe "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Lab6 where

import Data.List
import Data.Tree

import System.Random

import Types
--import DomViz
{- Uncomment the above line if you want to use the visualization routines. -}

board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }
--https://www.youtube.com/watch?v=WXuK6gekU1Y
-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }



--Note: sorry for the delay of the submission...

-- Exercise 1a
legalMoves :: Player -> Board -> [Cell]
legalMoves p b = [c | c <- free b , c `elem` free b && adjCell c p `elem` free b]
 --similar to a condition on the cell, we put the definition of 'valid' but replacing (turn b) by player p

-- Exercise 1b
moveLegal :: Board -> Cell -> Board
moveLegal b c =  Board { turn = opp (turn b) , 
                         free = [cell | cell <- free b, cell/=c && cell/= adjCell c (turn b)], 
                         hist = c:(hist b) }
-- we update the Board using the board constructor : 
--changing the turn, removing the move and its adjacent cell from the free cells and adding the move to hist


-- Exercise 1c

reverseMove :: Board -> Board
reverseMove b = Board {turn = opp (turn b),
                      free  = c: adjCell c (turn b): free b,
                      hist = tail (hist b) }
                      where c = head (hist b)
-- we create a function that undoes the effects of moveLegal

replay :: Board -> [Board]
replay board = replayAcc [board] (hist board)
-- we use an accumulator and the reverseMove function 

replayAcc :: [Board] -> [Cell] -> [Board]
replayAcc acc [] = acc
replayAcc acc (cell:cs) = replayAcc ((reverseMove (head acc)):acc) cs

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Exercise 2a
score :: Board -> Score
score b
    | (legalMoves H b == []) && (legalMoves V b == []) = Win (opp $ turn b)  
    | otherwise = Heu (length (legalMoves V b) - length (legalMoves H b) - sign (turn b))
  where
    sign H = -1
    sign V = 1
-- we check whether one of the players has no legal moves available which means that the other player wins
-- otherwise we compute the score

-- Exercise 2b
minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax = minimax f (Node b ts) = Node (b, score b) (map (minimax f) ts)
-- we associate each node of the tree with a pair (board state ,score)


-- Exercise 2c
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves = undefined
-- I did not manage to do this one

selectSafe :: SelectMonad m => [a] -> m (Maybe a)
selectSafe [] = return Nothing
selectSafe xs = select xs >>= \x -> return (Just x)
   
randomBestPlay :: SelectMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = selectSafe . bestmoves d sfn

randomPlay :: SelectMonad m => Board -> m (Maybe Cell)
randomPlay b = selectSafe (legalMoves (turn b) b)

-- Exercise 3a
runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame playH playV init = runGameMonad init
  where
    runGameMonad :: SelectMonad m => Board -> m Board
    runGameMonad b = do
      move <- (if (turn b == H) then playH else playV) b
      case move of
        Nothing -> return b
        Just cell -> do
          if valid  (moveLegal b cell) cell
            then runGameMonad (moveLegal b cell)
            else return b

-- Exercise 3b (optional)
carpets :: [Board]
carpets = undefined

-- alpha-beta pruning (optional)

