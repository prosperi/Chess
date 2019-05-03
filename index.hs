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
getColor (Pawn {color = c}) = c
getColor (Rook {color = c}) = c
getColor (Knight {color = c}) = c
getColor (Bishop {color = c}) = c
getColor (Queen {color = c}) = c
getColor (King {color = c}) = c

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
validate (Pawn {color = c}) (x0, y0) (x1, y1) board = True
validate (Rook {color = c}) (x0, y0) (x1, y1) board = if (x0 == x1 && y0 /= y1) || (x0 /= x1 && y0 == y1)
  then True
  else False
validate (Knight {color = c}) (x0, y0) (x1, y1) board = True
validate (Bishop {color = c}) (x0, y0) (x1, y1) board = True
validate (Queen {color = c}) (x0, y0) (x1, y1) board = True
validate (King {color = c}) (x0, y0) (x1, y1) board = True

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


play :: [[Piece]] -> IO ()
play board = do
  putStr $ draw $ board
  putStr "\n(r0, c0):"
  initial <- getLine
  putStr "(r1, c1):"
  final <- getLine
  putStr "\n"
  play $ move board (read initial :: (Int, Int)) (read final :: (Int, Int))

main = do play $ create
