Haskell Chess
-------------
This version of chess was implemented using the Haskell programming language and includes nearly all of the features found in a standard chess game. A full description of the implemented functionality is provided below.


Running the Game
-------------

To compile to an executable file, run ghc -o chess index.hs in the Chess directory.

To run the executable, run ./chess.

To run chess through the interpreter, run ghci index.hs and then type :main.

How to Play
----------------
There are black pieces and white pieces. Each player should pick a color. Then, when it is their turn they can only move their own color pieces. The white player always moves first followed by the black. The players then alternate turns. If a player tries to move when it is not their turn, the game will not accept their move.

To move a piece, enter the coordinates for the piece that you would like to move followed by the coordinates for the location that you would like to move the piece to. If the move is valid, the game will move the piece. If it is invalid, you will have a chance to try again. Rules for the moves can be found later in this document.

Example of a move:

![](https://i.imgur.com/iA2eKyF.png)

The game ends when a players king cannot move anywhere without being in "check." This state is called "checkmate." Check occurs when an opponents piece is in position to kill the king. When checkmate occurs, the player who is not in checkmate is declared the winner.

The Board
---------
Chess features an 8x8 board which players can move their pieces on. Any moves outside the bounds of the 8x8 board are invalid.

Starting Position
-----------------
All pieces must start on the board in a particular format.
The top row of the board features from left to right a rook, knight, bishop, queen, king, bishop, knight, and a rook from the black team. The next row below features eight pawns from the black team.

The second to bottom row features the eight pawns from the white team. This is followed by a row with a rook, knight, bishop, queen, king, bishop, knight, rook in left to right order.

Here is a graphical representation of the starting board. The definitions of each of the pieces are provided below.

![](https://i.imgur.com/e2o1OPI.png)

Piece Rules
-----------
Pawn ♟: 
From their starting position, the pawn can move forward one space or two spaces. From other positions, the pawn can only move forward one space. The only exception to this rule is that the pawn can move one space diagonally when killing an opponent's piece. A pawn cannot kill a piece by moving directly forward. If any piece is directly in front of a pawn, it is unable to move into that spot. Additionally, in a rule known as "pawn promotion," if a pawn makes it all the way across the board (in the vertical direction) it is promoted to a queen.

Bishop ♝:
The bishop can only move diagonally. If an opponent's piece is on its ending position, that piece is killed. No other pieces can be along the path or the move is not valid.

Knight ♞:
The night can move a combination of one and two spaces in the two cardinal directions. 
Examples: up two right one, down one left two.
The knight cannot move one right and one up or two right and two up. It must always feature a one space and a two space move in two cardinal directions. Any pieces in between the path of the movement do not matter. The only requirement is that your own piece is not present on the destination of the move. If an opponent's piece is present in the destination, then you kill it.

Rook ♜:
The rook can move either directly horizontally or directly vertically on the board. It can kill an opponent's piece if it is at its final destination. Any other pieces along the path invalidate the move.

King ♚:
The king can move one space in any direction (horizontally, vertically, or diagonally). The only requirements are that a piece of the king's color is not in the way and that the king is not moving into a position which would put it into check. If the king moves into a spot occupied by an opponent's piece, that piece is killed.

Queen ♛:
The queen can move either vertically, horizontally, or diagonally. No pieces can be along its path, other than an opponent's piece in the final position. In that case, the opponent's piece is eliminated.
