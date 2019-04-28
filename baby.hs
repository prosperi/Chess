data Cell = Empty
          | Pawn {symbol :: String}
          | Rook {symbol :: String}
          | Knight {symbol :: String}
          | Bishop {symbol :: String}
          | Queen {symbol :: String}
          | King {symbol :: String}
          deriving (Show)

create = (base "W")
        : (take 8 $ repeat Pawn {symbol = "PW"})
        : (take 6 $ repeat (take 8 $ repeat Empty))
        ++ (take 8 $ repeat Pawn {symbol = "PB"})
        : (reverse (base "B"))
        : []
        where base c = Rook {symbol = "R" ++ c}
                    :Knight {symbol = "K" ++ c}
                    :Bishop {symbol = "B" ++ c}
                    :Queen {symbol = "Q" ++ c}
                    :King {symbol = "K" ++ c}
                    :Bishop {symbol = "B" ++ c}
                    :Knight {symbol = "K" ++ c}
                    :Rook {symbol = "R" ++ c}
                    :[]

printer :: Cell -> String
printer Empty = "__"
printer (Pawn {symbol = c}) = c
printer (Rook {symbol = c}) = c
printer (Knight {symbol = c}) = c
printer (Bishop {symbol = c}) = c
printer (Queen {symbol = c}) = c
printer (King {symbol = c}) = c

draw :: [[Cell]] -> IO ()
draw board = putStr
            $ foldl1 (++)
            $ map (\r -> (
                foldl1 (++)
                $ map (++ "    ")
                $ map printer r
              )
              ++ "\n"
              ++ (foldl1 (++) $ take (8 * 6) $ repeat " ")
              ++ "\n"
            ) board