module TicTacToe (Board, Player, gameOver, winner, tie, move, initBoard, yourTurn) where

import Data.Maybe(isJust, isNothing)

data Player = Xer | Oer
	deriving (Eq, Show)

data Cell = X | O | E
	deriving (Eq, Show)

type Board = [Cell]

rows :: Board -> [[Cell]]
rows b = [[head b, b!!1, b!!2],
	  [b!!3, b!!4, b!!5],
	  [b!!6, b!!7, b!!8]]

columns :: Board -> [[Cell]]
columns b = [[head b, b!!3, b!!6],
	     [b!!1, b!!4, b!!7],
	     [b!!2, b!!5, b!!8]]

diagonals :: Board -> [[Cell]]
diagonals b = [[head b, b!!4, b!!8],
	       [b!!2, b!!4, b!!6]]

playerToCell :: Player -> Cell
playerToCell Xer = X
playerToCell Oer = O

playerToString :: Player -> Char
playerToString Xer = 'X'
playerToString Oer = 'O'

control :: (Board -> [[Cell]]) -> Board -> Player -> Bool
control f b p = c' spss c
	    where spss = f b
	    	  c = playerToCell p
		  c' [] _ = False
		  c' (sp:sps) c = all (c ==) sp || c' sps c

rowControl :: Board -> Player -> Bool
rowControl = control rows

columnControl :: Board -> Player -> Bool
columnControl = control columns

diagonalControl :: Board -> Player -> Bool
diagonalControl = control diagonals

win :: Board -> Player -> Bool
win b p = rowControl b p || columnControl b p || diagonalControl b p

winner :: Board -> Maybe Player
winner b | win b Xer = Just Xer
	 | win b Oer = Just Oer
	 | otherwise = Nothing

boardFull :: Board -> Bool
boardFull b = numEs b == 0

gameOver :: Board -> Bool
gameOver b = boardFull b || isNothing (winner b)

tie :: Board -> Bool
tie b = isNothing (winner b) && boardFull b

whoseTurn :: Board -> Player
whoseTurn b | even $ numEs b = Oer
	    | otherwise = Xer

numCells :: Board -> Cell -> Int
numCells bs c = length (filter (== c) bs)

numEs :: Board -> Int
numEs b = numCells b E

replace :: Board -> Int -> Player -> Board
replace b n p = replace' b n 0 c
	      where c = playerToCell p

replace' :: [a] -> Int -> Int -> a -> [a]
replace' [] _ _ _ = []
replace' (x:xs) n i r  | n == i = r:xs
		       | otherwise = x: replace' xs n (i + 1) r

move :: Board -> Int -> Maybe Board
move b n | b!!n /= E = Nothing
	 | n > 8 || n < 0 = Nothing
	 | gameOver b = Nothing
	 | otherwise = Just (replace b n p)
	 where p = whoseTurn b

yourTurn :: Board -> String
yourTurn board = "It is " ++ playerToString (whoseTurn board) : "'s turn"

initBoard :: Board
initBoard = [E, E, E,
	     E, E, E,
	     E, E, E]

randomBoard :: Board
randomBoard = [O, X, O,
	       X, O, E,
	       X, E, E]

oWin :: Board
oWin = [X, X, O,
	E, X, O,
	E, E, O]

xWin :: Board
xWin = [X, O, X,
	O, X, O,
	E, E, X]

tieBoard :: Board
tieBoard = [X, O, X,
	    X, O, O,
	    O, X, X]

xFullBoard :: Board
xFullBoard = [X, O, O,
	      X, O, O,
	      X, X, X]
