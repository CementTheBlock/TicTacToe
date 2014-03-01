module Main where

import TicTacToe

--Board
--Player
--gameOver
--winner
--tie
--move
--initBoard
--yourTurn

promptMove :: Board -> IO Int
promptMove b = do putStrLn (yourTurn b)
		  putStrLn "Where do you want to go?"
		  x <- getLine
		  return (read x :: Int)

--game :: (Board, Bool) -> IO ()
--game (x,p) = do mv <- promptMove x
--	        if p then game (move x mv, gameOver (move x mv)) else if tie x then print "Tie" else print (show (winner x))

main = do putStrLn "1 | 2 | 3"
	  putStrLn "__|___|__"
	  putStrLn "4 | 5 | 6"
	  putStrLn "__|___|__"
	  putStrLn "7 | 8 | 9"
