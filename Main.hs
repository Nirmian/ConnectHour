module Main where

import Data.Array
import System.Random

data Player = Red | Yellow deriving (Eq, Show)
data Cell = Empty | Filled Player deriving (Eq)
data State = Running | Gameover (Maybe Player) deriving (Eq, Show)

type Board = Array (Int, Int) Cell

data Game = Game { gameBoard :: Board
                 , gamePlayer :: Player
                 , gameState :: State
                 } deriving (Eq, Show)


instance Show Cell where
    show (Empty) = show "#"
    show (Filled Red) = show "X"
    show (Filled Yellow) = show "O"

rows :: Int
columns :: Int
rows = 6
columns = 7

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty])
                   , gamePlayer = Red
                   , gameState = Running
                   }
    where indexRange = ((0,0), (rows - 1, columns - 1))

--unwords [String] -> String takes every element from the list and returns a String with spaces between them
--unlines [String] -> String adds \n for every element from the list
drawBoard :: Board -> String
drawBoard board = unlines [unwords [show (board ! (x, y)) | y <- [0..columns - 1] ] | x <- [0..rows - 1]] ++ " 0   1   2   3   4   5   6"

is_column_free :: Board -> Int -> Bool
is_column_free board column = (board) ! (0,column) == Empty

get_free_row :: Board -> Int -> (Int, Int)
get_free_row board column = last [(x,column) | x<-[0..rows -1], (board ! (x,column))  == Empty]

is_valid_move :: Int -> Bool
is_valid_move column = if (column >= 0 && column <= 6) then True
                     else False

make_move :: Board -> Player -> Int -> Board
make_move board player column = if (is_column_free board column) then
                         board//[((get_free_row board column), Filled player)]
                         else board                   

get_opponent :: Cell -> Cell
get_opponent (Filled Red) = (Filled Yellow)
get_opponent (Filled Yellow) = (Filled Red)

playerTurn :: Player -> Player
playerTurn Red = Yellow
playerTurn Yellow = Red

full :: [Cell] -> Cell
full (cell@(Filled player):cells) | all (== cell) cells = Filled player
full _ = Empty

game_over :: Board -> Bool
game_over board = if (filter (\x -> x == Filled Yellow || x == Filled Red) (map full $ n_lines 4 board) /= []) then True
                else False

test :: Board
test = array ((0,0),(0,3)) [((0,0),(Filled Red)),((0,1),(Filled Red)),((0,2),(Filled Red)), ((0,3), Filled Yellow)]

is_tie :: Board -> Bool
is_tie board = if(count_elem Empty (elems board) == 0) then True
            else False

is_win_move :: Board -> Cell -> Bool
is_win_move board piece = if win_moves board piece (n_lines' 4 board) /= [] then True
                        else False

win_moves :: Board -> Cell -> [[(Int, Int)]] -> [(Int, Int)]
win_moves board piece [] = error "empty list"
win_moves board piece [x] = if (get_filled_tuples board piece x == 3) then get_empty_tuples board x 
                            else []
win_moves board piece (x:xs) = if (get_filled_tuples board piece x == 3) then 
                                    get_empty_tuples board x ++ win_moves board piece xs
                                else win_moves board piece xs

get_filled_tuples :: Board -> Cell -> [(Int, Int)] -> Int
get_filled_tuples board cell [] = error "empty list"
get_filled_tuples board cell [x] = if (is_cell_type board cell x) then 1
                            else 0
get_filled_tuples board cell (x:xs) = if is_cell_type board cell x then 1 + get_filled_tuples board cell xs
                                    else get_filled_tuples board cell xs

is_cell_type :: Board -> Cell -> (Int, Int) -> Bool
is_cell_type board cell tuple = if (board ! tuple == cell) then True
                                else False

get_empty_tuples :: Board -> [(Int, Int)] -> [(Int, Int)]
get_empty_tuples board [] = error "empty list"
get_empty_tuples board [x] = if is_cell_type board Empty x then [x]
                            else []
get_empty_tuples board (x:xs) = if is_cell_type board Empty x then [x] ++ get_empty_tuples board xs
                            else get_empty_tuples board xs

get_winner :: Player -> IO ()
get_winner player = if (player /= Red) then putStrLn "Red player wins"
                    else putStrLn "Yellow player wins"

n_lines :: Int -> Board -> [[Cell]]
n_lines len board =
    -- Get all horizontal lines
    [ [board ! (r, c + i) | i <- size]
    | r <- [0 .. rows - 1], c <- [0 .. columns - len]
    ] ++

    -- Get all vertical lines
    [ [board ! (r + i, c) | i <- size]
    | r <- [0 .. rows - len], c <- [0 .. columns - 1]
    ] ++

    -- Get all diagonal lines: top left to bottom right
    [ [board ! (r + i, c + i) | i <- size]
    | r <- [0 .. rows - len], c <- [0 .. columns - len]
    ] ++

    -- Get all diagonal lines: top right to bottom left
    [ [board ! (r + i, c - i) | i <- size]
    | r <- [0 .. rows - len], c <- [len - 1 .. columns - 1]
    ]
  where
    size = [0 .. len - 1]

n_lines' :: Int -> Board -> [[(Int, Int)]]
n_lines' len board =
    -- Get all horizontal lines
    [ [(r, c + i) | i <- size]
    | r <- [0 .. rows - 1], c <- [0 .. columns - len]
    ] ++

    -- Get all vertical lines
    [ [(r + i, c) | i <- size]
    | r <- [0 .. rows - len], c <- [0 .. columns - 1]
    ] ++

    -- Get all diagonal lines: top left to bottom right
    [ [(r + i, c + i) | i <- size]
    | r <- [0 .. rows - len], c <- [0 .. columns - len]
    ] ++

    -- Get all diagonal lines: top right to bottom left
    [ [(r + i, c - i) | i <- size]
    | r <- [0 .. rows - len], c <- [len - 1 .. columns - 1]
    ]
  where
    size = [0 .. len - 1]

center_line :: Board -> [Cell]
center_line board = [board ! (r, 3) | r <- [0 .. rows - 1]]

count_elem :: Eq a => a -> [a] -> Int
count_elem _ [] = 0
count_elem x list = sum $ map (\a -> 1) $ filter (== x) list

line_frequency :: (Eq a, Eq b) => b -> [((a, a), b)] -> Int
line_frequency _ [] = 0
line_frequency x list = sum $ map (\((_, _), b) -> 1) $ filter condition list
    where condition (_, cell) = (cell == x)

eval_line :: [Cell] -> Cell -> Int
eval_line line piece = if count_elem piece line == 4 then 100
                        else if count_elem piece line == 3 && count_elem Empty line == 1 then 5
                        else if count_elem piece line == 2 && count_elem Empty line == 2 then 2
                        else if count_elem (get_opponent piece) line == 3 && count_elem Empty line == 1 then -4
                        else 0

score_position :: Board -> [[Cell]] -> Player -> Int
score_position board [x] player = (eval_line x (Filled player)) + (count_elem (Filled player) (center_line board)) * 3
score_position board (x:xs) player = eval_line x (Filled player) + score_position board xs player

-- doesn't work properly. Recursion doesn't check score properly
pick_best_move :: Board -> Player -> [Int] -> Int -> Int
pick_best_move board player [x] = x
pick_best_move board player (x:xs) 
    | score_position board (n_lines 4 (make_move board player x)) player > score_position board (n_lines 4 (make_move board player maxTail)) player = x
    | otherwise = maxTail
    where maxTail = pick_best_move board player xs

is_terminal_node :: Board -> Bool
is_terminal_node board = is_win_move board (Filled Red) || is_win_move board (Filled Yellow) || is_tie board

-- minimax :: Board -> Int -> Player -> Int
-- minimax board depth maximizingPlayer = if (is_terminal_node board) then
--                                         if (is_win_move board (Filled Yellow)) then 999999
--                                         else if (is_win_move board (Filled Red)) then -999999
--                                         else 0
--                                     else score_position board (Yellow)

ai_move :: Board -> Player -> Int -> Board
ai_move board player column = make_move board player column

game_loop :: Board -> Player -> IO()
game_loop board player = do
    putStrLn $ id (drawBoard board)
    putStrLn $ show $ win_moves board (Filled Yellow) (n_lines' 4 board)
    putStrLn $ show $ score_position board (n_lines 4 board) player
    putStrLn $ show $ pick_best_move board Red [0,1,2,3,4,5,6]

    if (game_over board) then do
        putStrLn "Game over"
        get_winner player
        return ()
    else do

        if(is_tie board) then do
            putStrLn "Game ended in a tie"
            return ()
        else do
            if(player == Red) then do
                move <- getLine
                if(is_valid_move (read move :: Int) == True) then
                    game_loop (make_move board player (read move :: Int)) (playerTurn player)
                else
                    game_loop board player
            else do
                let bestMove = pick_best_move board Yellow [0,1,2,3,4,5,6]
                -- randgen <- getStdGen
                -- randomCol <- randomRIO (0,6 :: Int)
                if(is_valid_move bestMove == True) then
                    game_loop (ai_move board player bestMove) (playerTurn player)
                else
                    game_loop board player

main :: IO()
main = do 
    game_loop (gameBoard initialGame) Red