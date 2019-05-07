import Text.Read

{--
  Piece is a type with Empty, Pawn, Rook, Knight, Bishop,
  Queen and King value constructors. Each constructor takes
  one parameter as argument - color. The Piece type derives
  from Show and Eq, meaning that we display and check the values
  for equality.
--}
data Piece = Empty {color :: String}
          | Pawn {color :: String}
          | Rook {color :: String}
          | Knight {color :: String}
          | Bishop {color :: String}
          | Queen {color :: String}
          | King {color :: String}
          deriving (Show, Eq)

{--
  create - generates the initial state of the board, with all pieces at their
  initial positions.
--}
create :: [[Piece]]
create = base "W"
        : pawns "W"
        : empty
        : reverse empty
        : empty
        : reverse empty
        : pawns "B"
        : base "B"
        : []
        -- construct the empty cells with alternating color and use fold to construct a whole row
        where empty = foldl1 (++) (take 4 $ repeat [Empty {color = "B"}, Empty {color = "W"}])
              -- generate 8 pawns
              pawns c = take 8 $ repeat Pawn {color = c}
              -- construct the base row: ♖♘♗♕♔♗♘♖
              base c = Rook {color = c}
                      : Knight {color = c}
                      : Bishop {color = c}
                      : Queen {color = c}
                      : King {color = c}
                      : Bishop {color = c}
                      : Knight {color = c}
                      : Rook {color = c}
                      : []
{--
  move method takes the board, initial position of the piece, the final
  position of the piece and moves the piece to that ceil if it is possible
  based on the rules. In the move returns a new board with updated state of
  the game
--}
move :: String -> [[Piece]] -> (Int, Int) -> (Int, Int) -> [[Piece]]
move turn board (x0, y0) (x1, y1) = if (validate (board !! x0 !! y0) (x0, y0) (x1, y1) board) && ((getCell board (x0,y0)) == turn)
  then
    map (\r ->
      map
        (\v ->
          if (fst v) == y0 && (fst r) == x0 then
            -- update the initial position to empty
            if (x0 + y0) `mod` 2 == 0 then Empty {color = "B"}
            else Empty {color = "W"}
          -- place piece to its new position
          else if (fst v) == y1 && (fst r) == x1 then (board !! x0 !! y0)
          else (snd v)
        )
        (zip [0..] (snd r)) -- use zippers to get the correct index of the column
    )
    $ zip [0..] board -- use zippers to get the correct index of the row
  else
    board

{--
  getCell provides the color of the piece in given cell on given board
  or returns string "Empty" if the cell is empty
--}
getCell :: [[Piece]] -> (Int, Int) -> String
getCell board (x, y) = getColor(board !! x !! y)

{--
  get the color of the piece
--}
getColor :: Piece -> String
getColor (Empty {color = c}) = "Empty"
getColor piece = color piece

{--
  Draw the board in a nice way. draw function takes board
  as an argument and returns string representation for it
--}
draw :: [[Piece]] -> String
draw board = foldl1 (++)
  -- provide an easy way to distinguish between columns
  $ (++ ["   0  1  2  3  4  5  6  7\n"])
  $ map (\(index, row) ->
    show index
    ++ " "
    ++ (
      foldl1 (++)
      $ map (\v -> " " ++ v ++ " ")
      $ map format row
    )
    ++ "\n"
    -- generate extra space between rows
    ++ (foldl1 (++) $ take (8 * 3) $ repeat " ")
    ++ "\n"
  )
  $ zip [7, 6..]
  $ reverse board

{--
  validate function takes piece, its initial position and final position, and the
  board as arguments and returns True if piece can move to that final position, or
  False if piece cannot move to that position. validate uses guards to match with
  different value constructors
--}
validate :: Piece -> (Int, Int) -> (Int, Int) -> [[Piece]] -> Bool

-- Empty cells cannot move, hence return False always
validate (Empty {color = c}) (x0, y0) (x1, y1) board = False

-- Provides rules for pawns
validate (Pawn {color = c}) (x0, y0) (x1, y1) board =
  -- if pawn is White
  if ((getCell board (x0, y0)) == "W")
    --check for double move case
    then if (x0 == 1 && y0 == y1 && x1 == 3) then if ((getCell board (x1, y1)) /= c && (getCell board (2, y1)) /= c) then True else False
    -- move forward one space
    else if (x0 == (x1 - 1) && y0 == y1) then if ((getCell board (x1, y1)) == "Empty") then True else False
    -- take the diagonal enemy piece
    else if (x0 == (x1 - 1) && y0 == (y1 - 1)) then if ((getCell board (x1, y1)) == "B") then True else False
    else if (x0 == (x1 - 1) && y0 == (y1 + 1)) then if ((getCell board (x1, y1)) == "B") then True else False
    else False --invalid move by White
  -- if pawn is Black
  else if ((getCell board (x0, y0)) == "B")
    -- check for double move case
    then if (x0 == 6 && y0 == y1 && x1 == 4) then if ((getCell board (x1, y1)) /=c && (getCell board (5, y1)) /= c) then True else False
    -- move forward one space
    else if (x0 == (x1 + 1) && y0 == y1) then if ((getCell board (x1, y1)) == "Empty") then True else False
    -- take the diagonal enemy piece
    else if (x0 == (x1 + 1) && y0 == (y1 - 1)) then if ((getCell board (x1, y1)) == "W") then True else False
    else if (x0 == (x1 + 1) && y0 == (y1 + 1)) then if ((getCell board (x1, y1)) == "W") then True else False
    else False --invalid move by Black
  -- backup fail state, should never be reached
  else False

-- Provides rules for Rook
validate (Rook {color = c}) (x0, y0) (x1, y1) board =
  if (x0 == x1 && y0 /= y1) || (x0 /= x1 && y0 == y1)
  then if ((getCell board (x1, y1)) /= c) then isPathClearLinear board (x0, y0) (x1, y1) else False
  else False
  -- check knight move

-- Provide rules for Knight
validate (Knight {color = c}) (x0, y0) (x1, y1) board =
  --checks each of the eight possible moves for a knight
  if (x0 == (x1 + 2) && y0 == (y1 + 1)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 + 2) && y0 == (y1 - 1)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 - 2) && y0 == (y1 + 1))  then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 - 2) && y0 == (y1 - 1)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 + 1) && y0 == (y1 + 2)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 + 1) && y0 == (y1 - 2)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 - 1) && y0 == (y1 + 2)) then if ((getCell board (x1, y1)) /= c) then True else False
  else if (x0 == (x1 - 1) && y0 == (y1 - 2)) then if ((getCell board (x1, y1)) /= c) then True else False
  else False --invalid move for knight

-- Provide rules for Bishop
validate (Bishop {color = c}) (x0, y0) (x1, y1) board =
    if ((abs (x0 - x1)) == (abs (y0 - y1))) then if ((getCell board (x1, y1)) /= c) then isPathClearDiagonal board (x0, y0) (x1, y1) else False
    else False

-- Provide rules for Queen
validate (Queen {color = c}) (x0, y0) (x1, y1) board =
    if ((getCell board (x1, y1)) /= c)
        then if (x0 == x1 && y0 /= y1) || (x0 /= x1 && y0 == y1) then isPathClearLinear board (x0, y0) (x1, y1)
            else if ((abs (x0 - x1)) == (abs (y0 - y1))) then isPathClearDiagonal board (x0, y0) (x1, y1) else False
    else False

-- Provide rules for King
validate (King {color = c}) (x0, y0) (x1, y1) board =
    if (((abs (x0 - x1)) < 2) && ((abs (y0 - y1)) < 2)) then if ((getCell board (x1, y1)) /= c) then True else False
    else False

isPathClearLinear :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Bool
isPathClearLinear board (x0, y0) (x1, y1) =
    if ((x0 == x1) && (y0 == y1)) then True
    else
        if(x0 > x1) then if ((getCell board ((x0 - 1), y1)) == "Empty") || (((x0 - 1) == x1) && (y0 == y1)) then isPathClearLinear board (x0-1, y0) (x1, y1) else False
        else if(x0 < x1) then if ((getCell board ((x1 - 1), y1)) == "Empty") || ((x0 == (x1 - 1)) && (y0 == y1)) then isPathClearLinear board (x0, y0) (x1-1, y1) else False
        else if(y0 > y1) then if ((getCell board (x1, (y0 - 1))) == "Empty") || ((x0 == x1) && ((y0 - 1) == y1)) then isPathClearLinear board (x0, y0-1) (x1, y1) else False
        else if ((getCell board (x1, (y1 - 1))) == "Empty") || ((x0 == x1) && (y0 == (y1 - 1))) then isPathClearLinear board (x0, y0) (x1, y1-1) else False

isPathClearDiagonal :: [[Piece]] -> (Int, Int) -> (Int, Int) -> Bool
isPathClearDiagonal board (x0, y0) (x1, y1) =
    if ((x0 == x1) && (y0 == y1)) then True
    else
        if(x0 > x1) && (y0 > y1) then if ((getCell board ((x0 - 1), (y0 - 1))) == "Empty") || (((x0 - 1) == x1) && ((y0 - 1) == y1)) then isPathClearDiagonal board (x0 - 1, y0 - 1) (x1, y1) else False
        else if(x0 > x1) && (y0 < y1) then if ((getCell board ((x0 - 1), (y0 + 1))) == "Empty") || (((x0 - 1) == x1) && ((y0 + 1) == y1)) then isPathClearDiagonal board (x0 - 1, y0 + 1) (x1, y1) else False
        else if(x0 < x1) && (y0 > y1) then if ((getCell board ((x0 + 1), (y0 - 1))) == "Empty") || (((x0 + 1) == x1) && ((y0 - 1) == y1)) then isPathClearDiagonal board (x0 + 1, y0 - 1) (x1, y1) else False
        else if ((getCell board ((x0 + 1), (y0 + 1))) == "Empty") || (((x0 + 1) == x1) && (y1 == (y0 + 1))) then isPathClearDiagonal board (x0 + 1, y0 + 1) (x1, y1) else False

{--
  inCheck checks if the provided board has a king with provided color in check.
  It accomplishes this by going through each piece of opposite color and checking if
  at least one of them threatens to the king
--}
inCheck :: String -> [[Piece]] -> Bool
inCheck turn board = foldl
    (\acc (index, piece) -> acc || validate piece (div index 8, mod index 8) (div king 8, mod king 8) board)
    False
    (zip [0..] (concat board))
  where king = fst $ (filter (\(index, cell) -> cell == King {color = turn}) $ zip [0..] (concat board)) !! 0

{--
  inCheckMate checks if there is a checkmate based on the given board and piece color.
  It accomplishes this by going through each piece of current color while king is in check
  and checks if any piece could move to any cell on the board that would save the king
--}
inCheckMate :: String -> [[Piece]] -> Bool
inCheckMate turn board =
  inCheck turn board
  &&
  foldl
    (\acc (index, piece) ->
      acc
      &&
      foldl
        (&&)
        True
        (map (\cell -> inCheck turn $ move turn board (div index 8, mod index 8) (div cell 8, mod cell 8)) [0,1..63])
    )
    True
    (filter (\(index, piece) -> getColor piece == turn) (zip [0..] (concat board)))



format :: Piece -> String
format (Empty {color = c})
  | c == "B" = "▓"
  | c == "W" = "░"
format (Pawn {color = c})
  | c == "B" = "♟"
  | c == "W" = "♙"
format (Rook {color = c})
  | c == "B" = "♜"
  | c == "W" = "♖"
format (Knight {color = c})
  | c == "B" = "♞"
  | c == "W" = "♘"
format (Bishop {color = c})
  | c == "B" = "♝"
  | c == "W" = "♗"
format (Queen {color = c})
  | c == "B" = "♛"
  | c == "W" = "♕"
format (King {color = c})
  | c == "B" = "♚"
  | c == "W" = "♔"

validateInput :: String -> IO (Int, Int)
validateInput prompt = do
  putStr prompt
  response <- getLine
  let parsed = readMaybe response :: Maybe (Int, Int)
  case parsed of
    Just parsed -> return parsed
    Nothing -> putStrLn "Invalid input. Please enter your position as (row, column)." >> validateInput prompt

changeColor :: String -> String
changeColor color = if color == "B" then "W" else "B"

play :: String -> Int -> [[Piece]] -> IO ()
play turn (-1) board = do
  putStr("\nGood game!\n")
play turn step board = do
  putStr $ draw $ board
  if inCheckMate turn board
    then do
      putStr "\nYou are finished!"
      play "B" (-1) board
    else if inCheck turn board
      then putStr "\nYou are in check!"
      else do
        putStr "\nPlay wisely!"
        initial <- validateInput "\n(r0, c0):"
        final <- validateInput "\n(r1, c1):"
        putStr "\n"
        play (changeColor turn) (step + 1) $ move turn board initial final

main = do play "W" 0 $ create
