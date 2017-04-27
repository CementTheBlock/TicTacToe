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

-- b = initBoard
-- print board with coord system
-- while not (gameOver b)
       -- print b
       -- output yourTurn b
       -- n = input
       -- if move b n = Nothing
              -- print "Please make sure your number is between 0 and 9 and specifies an empty space."
              -- continue
       -- if move b n = Just nb
              -- b = nb
--if tie b output "Tie"
--else output playerToString winner b

game::IO ()
game = _

main:: IO ()
main = do putStrLn "0 | 1 | 2"
          putStrLn "3 | 4 | 5"
          putStrLn "6 | 7 | 8"
          game
