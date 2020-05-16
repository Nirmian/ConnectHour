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
   show (Filled Red) = show "x"
   show (Filled Yellow) = show "o"

rows :: Int
columns :: Int
rows = 7
columns = 6

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (cycle [Empty])
                   , gamePlayer = Red
                   , gameState = Running
                   }
    where indexRange = ((0,0), (rows - 1, columns - 1))

--unwords [String] -> String takes every element from the list and returns a String with spaces between them
--unlines [String] -> String adds \n for every element from the list
drawBoard :: Board -> String
drawBoard board = unlines [unwords [show (board ! (x, y)) | y <- [0..columns - 1] ] | x <- [0..rows - 1]]

is_column_free :: Board -> Int -> Bool
is_column_free board column = (board) ! (0,column) == Empty

get_free_row :: Board -> Int -> (Int, Int)
get_free_row board column = last [(x,column) | x<-[0..rows -1], (board ! (x,column))  == Empty]

is_valid_move :: Int -> Bool
is_valid_move column = if (column >= 0 && column < 6) then True
                     else False

make_move :: Board -> Player -> Int -> Board
make_move board player column = if (is_column_free board column) then
                         board//[((get_free_row board column), Filled player)]
                         else board                   

playerTurn :: Player -> Player
playerTurn Red = Yellow
playerTurn Yellow = Red

full :: [Cell] -> Cell
full (cell@(Filled player):cells) | all (== cell) cells = Filled player
full _ = Empty

game_over :: Board -> Bool
game_over board = if (filter (\x -> x == Filled Yellow || x == Filled Red) (map full $ n_row 4 board) /= []) then True
                else False

get_winner :: Player -> IO ()
get_winner player = if (player /= Red) then putStrLn "Red player wins"
                    else putStrLn "Yellow player wins"

n_row :: Int -> Board -> [[Cell]]
n_row len board =
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

ai_move :: Board -> Player -> Int -> Board
ai_move board player column = make_move board player column

game_loop :: Board -> Player -> IO()
game_loop board player = do
    putStrLn $ drawBoard board

    if (game_over board) then do
        putStrLn "Game over"
        get_winner player
        return ()
    else do
        -- putStrLn "Game isn't over"       
        if(player == Red) then do
            move <- getLine
            if(is_valid_move (read move :: Int) == True) then
                game_loop (make_move board player (read move :: Int)) (playerTurn player)
            else
                game_loop board player
        else do
            randgen <- getStdGen
            randomCol <- randomRIO (0,5 :: Int)
            if(is_valid_move randomCol == True) then
                game_loop (ai_move board player randomCol) (playerTurn player)
            else
                game_loop board player

main :: IO()
main = do 
    game_loop (gameBoard initialGame) Red