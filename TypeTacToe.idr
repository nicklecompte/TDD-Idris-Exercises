module TicTacToe

-- Improving https://gitlab.com/snippets/1673837


import Data.Vect

||| All possible player pieces, where N represents empty
data Piece = X | O

||| Select next player 
next : Piece -> Piece
next X = O
next O = X

data BoardPositionState =
    Empty |
    Filled Piece


||| Type representing a 3x3 board
Board : Type
Board = Vect 3 $ Vect 3 BoardPositionState

||| A turn holds a board and has to be played by a certain player.
data Turn : (player : Piece) -> Type where
  MkTurn : (board : Board) -> Turn player

updateBoard : (coord: (Fin 3, Fin 3)) -> (piece: Piece) -> Board -> Maybe Board
updateBoard (a, b) piece xs = case (index b (index a xs)) of
                                Empty => ?rhs_1
                                (Filled x) => ?rhs_2

-- ||| A new game, where player X starts on an empty board.
-- newGame : Turn X
-- newGame = MkTurn emptyBoard
--   where emptyBoard = [ [N, N, N]
--                      , [N, N, N]
--                      , [N, N, N] ]
             
-- ||| Place a player's piece in the given coordinates, and return the updated
-- ||| board for the next player's turn.
-- play : (Fin 3, Fin 3) -> Turn player -> Turn (next player)
-- play {player} (x, y) (MkTurn board) = MkTurn board'
--   where board' = updateAt y (updateAt x (place player)) board

-- ||| Convenience function to play and represent games
-- (>>) : Turn p -> (Fin 3, Fin 3) -> Turn (next p)
-- (>>) b cs = play cs b
