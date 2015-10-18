module Gomoku

import StdEnv, StdLib

:: Direction = Horizontal | Vertical | LeftDiagonal | RightDiagonal
:: Player = X | O
:: Position :== (Int, Int)
:: Mark :== (Player, Position)
:: Board :== [Mark]
:: GameState :== (Player, Board)

// Szukseges, hogy az (==) fuggvenyt hasznalhassuk Player tipusu ertekekre.
instance == Player where
  (==) X X = True
  (==) O O = True
  (==) _ _ = False

// Szukseges, hogy az toString fuggvenyt hasznalhassuk Player tipusu ertekekre.
instance toString Player where
  toString X = "X"
  toString O = "O"

// Szukseges, hogy az (==) fuggvenyt hasznalhassuk Direction tipusu ertekekre.
instance == Direction where
  (==) Horizontal Horizontal = True
  (==) Vertical Vertical = True
  (==) LeftDiagonal LeftDiagonal = True
  (==) RightDiagonal RightDiagonal = True
  (==) _ _ = False

Start = (and (flatten allTests), allTests)
  where
    allTests =
      [ test_nextPlayer
      , test_emptyGame
      , test_nextStep
      //, test_checkNInARow
      , test_lineFilter
      , test_project
      //, test_checkBoardForN
      //, test_playGame
      //, test_areValidSteps
      //, test_startGame
      ]

nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X

test_nextPlayer :: [Bool]
test_nextPlayer =
  [ nextPlayer X == O
  , nextPlayer O == X
  ]

emptyGame :: GameState
emptyGame = (X, [])

test_emptyGame :: [Bool]
test_emptyGame =
  [ emptyGame == (X, [])
  ]


nextStep :: Position GameState -> GameState
nextStep (x, y) (player, board) = (nextPlayer(player), board ++ [(player, (x, y))])

test_nextStep :: [Bool]
test_nextStep =
  [ nextStep (1,1) emptyGame == (O,[(X,(1,1))])
  , foldr (\x y -> x == (O,(1,2)) || y) False (snd (nextStep (1,2) (O,[(X,(2,1))])))
  ]
/*
checkNInARow :: Int [Int] -> Bool
checkNInARow = undef

test_checkNInARow :: [Bool]
test_checkNInARow =
  [ checkNInARow 3 [3,1,~2,8,2]        == True
  , checkNInARow 4 [3,1,~2,8,2,99,~42] == False
  , checkNInARow 1 [3]                 == True
  , checkNInARow 5 []                  == False
  , checkNInARow 4 [0,1,42,~2,~1,99,8] == True
  , checkNInARow 3 [3,1,~2,1,2,8,2]    == True
  , checkNInARow 4 [3,1,~2,1,2,8,2]    == False
  ]
*/

lineFilter :: Mark Direction -> (Mark -> Bool)
lineFilter (player1, (x1,y1)) direction = isOnLine 
where
  isOnLine :: Mark -> Bool
  isOnLine (player2, (x2,y2))
    | direction == Horizontal = player1 == player2 && x1 == x2
    | direction == Vertical = player1 == player2 && y1 == y2
    | direction == LeftDiagonal = player1 == player2 && x1-y1 == x2-y2
    | direction == RightDiagonal = player1 == player2 && x1+y1 == x2+y2

test_lineFilter :: [Bool]
test_lineFilter =
  [ lineFilter (X,(3,3)) Horizontal (X,(3,3))     == True
  , lineFilter (X,(3,3)) Horizontal (X,(3,5))     == True
  , lineFilter (X,(3,3)) Horizontal (O,(0,0))     == False
  , lineFilter (X,(3,3)) Horizontal (X,(0,0))     == False
  , lineFilter (O,(0,0)) Vertical (O,(0,0))       == True
  , lineFilter (O,(0,0)) Vertical (X,(0,0))       == False
  , lineFilter (O,(0,0)) Vertical (O,(5,0))       == True
  , lineFilter (O,(3,3)) Vertical (O,(2,2))       == False
  , lineFilter (O,(0,0)) Vertical (O,(0,5))       == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(0,0))   == True
  , lineFilter (X,(0,0)) LeftDiagonal (O,(0,0))   == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(3,3))   == True
  , lineFilter (X,(0,0)) LeftDiagonal (O,(2,2))   == False
  , lineFilter (X,(0,0)) LeftDiagonal (X,(3,0))   == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(0,0))  == True
  , lineFilter (X,(0,0)) RightDiagonal (O,(0,0))  == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(~3,3)) == True
  , lineFilter (X,(0,0)) RightDiagonal (O,(~3,3)) == False
  , lineFilter (X,(0,0)) RightDiagonal (X,(3,3))  == False
  ]

project :: Direction -> (Mark -> Int)
project direction = getCoordinate
where
  getCoordinate :: Mark -> Int
  getCoordinate (player, (x,y))
    | direction == Horizontal = y
    | otherwise = x

test_project :: [Bool]
test_project =
  [ project Horizontal (X,(1,2))    == 2
  , project Vertical (O,(1,2))      == 1
  , project LeftDiagonal (X,(1,2))  == 1
  , project RightDiagonal (O,(1,2)) == 1
  ]
/*
checkBoardForN :: Int Mark Board Direction -> Maybe Player
checkBoardForN length (player, (x,y)) board direction =

test_checkBoardForN :: [Bool]
test_checkBoardForN =
  [ checkBoardForN 5 (O,(0,5)) [(X,(10,10)),(O,(0,4)),(X,(4,4)),(O,(0,3)),(X,(3,3)),(O,(0,2)),(X,(2,2)),(O,(0,1)),(X,(1,1))] Horizontal == Just O
  , checkBoardForN 3 (X,(0,0)) [(O,(0,2)),(X,(-1,-1)),(O,(0,1)),(X,(-2,-2))] LeftDiagonal  == Just X
  , checkBoardForN 3 (X,(0,0)) [(O,(0,2)),(X,(-1,-1)),(O,(0,1)),(X,(-2,-2))] RightDiagonal == Nothing
  , checkBoardForN 3 (X,(2,3)) [(O,(1,1)),(X,(1,4)),(O,(0,0)),(X,(0,5))] RightDiagonal     == Just X
  , checkBoardForN 2 (X,(0,0)) [(O,(1,1)),(X,(-1,0))] Vertical                             == Just X
  , checkBoardForN 2 (X,(1,0)) [(O,(1,1)),(X,(-1,0))] Vertical                             == Nothing
  ]

playGame :: Int [Position] -> (Maybe Player)
playGame = undef

test_playGame :: [Bool]
test_playGame =
  [ playGame 5 [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == Just O
  , playGame 3 [(~2,~2),(0,1),(~1,~1),(0,2),(0,0)]   == Just X
  , playGame 3 [(0,5),(0,0),(1,4),(1,1),(2,3)]       == Just X
  , playGame 3 [(0,5),(0,0),(1,4),(1,1)]             == Nothing
  , playGame 3 [(1,0),(1,1),(2,0),(2,2),(4,0),(3,3)] == Just O
  ]

areValidSteps :: [Position] -> Bool
areValidSteps [] = True
areValidSteps [x:xs] = until [x] == take (inc idx) xs
where idx = 0

test_areValidSteps :: [Bool]
test_areValidSteps =
  [ areValidSteps []                                     == True
  , areValidSteps [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == True
  , areValidSteps [(0,1),(3,4),(11,99),(2,3),(1,2)]      == True
  , areValidSteps [(~1,~2),(~2,~1),(10,5),(1,1),(~2,~1)] == False
  , areValidSteps [(~1,~10),(~1,~10),(1,1),(1,1)]        == False
  ]

startGame :: Int [Position] -> String
startGame = undef

test_startGame :: [Bool]
test_startGame =
  [ startGame 5 [(1,1),(0,1),(2,2),(0,2),(3,3),(0,3),(4,4),(0,4),(10,10),(0,5)] == "A jatekot nyerte: O"
  , startGame 3 [(~2,~2),(0,1),(~1,~1),(0,2),(0,0)]   == "A jatekot nyerte: X"
  , startGame 3 [(0,5),(0,0),(1,4),(1,1),(2,3)]       == "A jatekot nyerte: X"
  , startGame 3 [(0,5),(0,0),(1,4),(1,1)]             == "Nincs nyertes."
  , startGame 3 [(1,0),(1,1),(2,0),(2,2),(4,0),(3,3)] == "A jatekot nyerte: O"
  , startGame 4 [(1,1),(1,1),(2,0),(2,2),(4,0),(3,3)] == "Hibas lepessorozat."
  ]*/
