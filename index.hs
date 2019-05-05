import Text.Read

data Piece = Empty {color :: String}
          | Pawn {color :: String}
          | Rook {color :: String}
          | Knight {color :: String}
          | Bishop {color :: String}
          | Queen {color :: String}
          | King {color :: String}
          deriving (Show, Eq)

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
        where empty = foldl1 (++) (take 4 $ repeat [Empty {color = "B"}, Empty {color = "W"}])
              pawns c = take 8 $ repeat Pawn {color = c}
              base c = Rook {color = c}
                      : Knight {color = c}
                      : Bishop {color = c}
                      : Queen {color = c}
                      : King {color = c}
                      : Bishop {color = c}
                      : Knight {color = c}
                      : Rook {color = c}
                      : []

move :: [[Piece]] -> (Int, Int) -> (Int, Int) -> [[Piece]]
move board (x0, y0) (x1, y1) = if (validate (board !! x0 !! y0) (x0, y0) (x1, y1) board)
  then
    map (\r ->
      map
        (\v ->
          if (fst v) == y0 && (fst r) == x0 then
            if (x0 + y0) `mod` 2 == 0 then Empty {color = "B"}
            else Empty {color = "W"}
          else if (fst v) == y1 && (fst r) == x1 then (board !! x0 !! y0)
          else (snd v)
        )
        (zip [0..] (snd r))
    )
    $ zip [0..] board
  else
    board

getCell :: [[Piece]] -> (Int, Int) -> String
getCell board (x, y) = getColor(board !! x !! y)

getColor :: Piece -> String
getColor (Empty {color = c}) = "Empty"
getColor piece = color piece

draw :: [[Piece]] -> String
draw board = foldl1 (++)
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
    ++ (foldl1 (++) $ take (8 * 3) $ repeat " ")
    ++ "\n"
  )
  $ zip [7, 6..]
  $ rotate board

rotate :: [[Piece]] -> [[Piece]]
rotate = reverse

validate :: Piece -> (Int, Int) -> (Int, Int) -> [[Piece]] -> Bool
validate (Empty {color = c}) (x0, y0) (x1, y1) board = False
-- check pawn move

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

validate (Rook {color = c}) (x0, y0) (x1, y1) board =
  if (x0 == x1 && y0 /= y1) || (x0 /= x1 && y0 == y1)
  then if ((getCell board (x1, y1)) /= c) then isPathClearLinear board (x0, y0) (x1, y1) else False
  else False
  -- check knight move

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

validate (Bishop {color = c}) (x0, y0) (x1, y1) board =
    if ((abs (x0 - x1)) == (abs (y0 - y1))) then if ((getCell board (x1, y1)) /= c) then isPathClearDiagonal board (x0, y0) (x1, y1) else False
    else False

validate (Queen {color = c}) (x0, y0) (x1, y1) board =
    if ((getCell board (x1, y1)) /= c)
        then if (x0 == x1 && y0 /= y1) || (x0 /= x1 && y0 == y1) then isPathClearLinear board (x0, y0) (x1, y1)
            else if ((abs (x0 - x1)) == (abs (y0 - y1))) then isPathClearDiagonal board (x0, y0) (x1, y1) else False
    else False

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

inCheck :: [[Piece]] -> Bool
inCheck board = foldl (\acc (index, piece) -> acc || validate piece (div index 8, mod index 8) (div king 8, mod king 8) board) False (zip [0..] (concat board))
  where king = fst $ (filter (\(index, cell) -> cell == King {color = "W"}) $ zip [0..] (concat board)) !! 0

inCheckMate :: [[Piece]] -> Bool
inCheckMate board =
  inCheck board
  &&
  foldl
    (\acc (index, piece) ->
      acc
      &&
      foldl
        (&&)
        True
        (map (\cell -> inCheck $ move board (div index 8, mod index 8) (div cell 8, mod cell 8)) [0,1..63])
    )
    True
    (filter (\(index, piece) -> getColor piece == "W") (zip [0..] (concat board)))



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

play :: [[Piece]] -> IO ()
play board = do
  putStr $ draw $ board
  if inCheckMate board
    then putStr "\nYou are finished!"
    else if inCheck board
      then putStr "\nYou are in check!"
      else putStr "\nPlay wisely!"
  initial <- validateInput "\n(r0, c0):"
  final <- validateInput "\n(r1, c1):"
  putStr "\n"
  play $ move board initial final

main = do play $ create
