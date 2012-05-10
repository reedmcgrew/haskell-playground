-- exercize 3.2 in Haskell Book
-- All of these functions do the exact same thing, but in different ways--they join a list of lists with a 'glue' element

-- Without Guards
joinLists :: a -> [[a]] -> [a]
joinLists glue (piece:[]) = piece
joinLists glue (piece:pieces) = piece ++ [glue] ++ joinLists glue pieces

-- With Guards
joinLists2 :: a -> [[a]] -> [a]
joinLists2 glue pieces
    | length pieces <= 0    = []
    | length pieces == 1    = head pieces
    | length pieces >  1    = (head pieces) ++ [glue] ++ joinLists2 glue (tail pieces)

-- With Guards and Where statement
joinLists3 :: a -> [[a]] -> [a]
joinLists3 glue pieces
    | len <= 0    = []
    | len == 1    = firstPiece
    | len > 1     = firstPiece ++ [glue] ++ joinLists3 glue (tail pieces)
    where len = length pieces
          firstPiece = head pieces

-- Alternative Function Style
joinLists4 glue pieces = case pieces of
                            (x:[]) -> x
                            (x:xs) -> x ++ [glue] ++ joinLists4 glue xs

-- Alternative Function Style with Guards
joinLists5 glue pieces = case pieces of
                            pieces
                            | len <= 0    -> []
                            | len == 1    -> firstPiece
                            | len > 1     -> firstPiece ++ [glue] ++ joinLists5 glue (tail pieces)
                            where len = length pieces
                                  firstPiece = head pieces
