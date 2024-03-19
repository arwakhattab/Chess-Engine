import Data.List
import Data.Char

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

setBoard :: Board
setBoard = (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
			Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
			P ('h',2),P ('g',2),P ('f',2),P ('e',2),
			P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
			[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
			Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
			P ('h',7),P ('g',7),P ('f',7),P ('e',7),
			P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

visualizeBoard :: Board->String
visualizeBoard board = "    a    b    c    d    e    f    g    h\n" ++ "8 |" ++ (rowHelper 8 board) ++ "7 |" ++ (rowHelper 7 board) ++ "6 |" ++ (rowHelper 6 board)
						++ "5 |" ++ (rowHelper 5 board) ++ "4 |" ++ (rowHelper 4 board) ++ "3 |" ++ (rowHelper 3 board) ++ 
						"2 |" ++ (rowHelper 2 board) ++ "1 |" ++ (rowHelper 1 board) ++ (turnHelper board)

boardSearchWhite _ _ [] = ""
boardSearchWhite c n (h:t) | h == P (c, n) = " PW |"
						   | h == N (c, n) = " NW |"
						   | h == K (c, n) = " KW |"
						   | h == Q (c, n) = " QW |"
					       | h == R (c, n) = " RW |"
					       | h == B (c, n) = " BW |" 
					       | otherwise = boardSearchWhite c n t

boardSearchBlack _ _ [] = ""
boardSearchBlack c n (h:t) | h == P (c, n) = " PB |"
						   | h == N (c, n) = " NB |"
						   | h == K (c, n) = " KB |"
						   | h == Q (c, n) = " QB |"
					       | h == R (c, n) = " RB |"
					       | h == B (c, n) = " BB |" 
					       | otherwise = boardSearchBlack c n t

boardSearch c n (_,l1,l2) | boardSearchWhite c n l1 /= "" = boardSearchWhite c n l1
						  | boardSearchBlack c n l2 /= "" = boardSearchBlack c n l2
						  | otherwise = "    |"

rowHelper n board = (boardSearch 'a' n board) ++ (boardSearch 'b' n board) ++ (boardSearch 'c' n board) 
					++ (boardSearch 'd' n board) ++ (boardSearch 'e' n board) ++ (boardSearch 'f' n board) ++ 
					(boardSearch 'g' n board) ++ (boardSearch 'h' n board) ++ "\n"

turnHelper (White,_,_) = "\nTurn: White"
turnHelper (Black,_,_) = "\nTurn: Black"

isWhite piece (_,l1,_) = elem piece l1

pawnHelper (P (c1,n1)) board (c2,n2) | n2 > 8 || n2 < 1 = False
									 | isWhite (P (c1,n1)) board = pawnWhiteHelper (c1,n1) (c2,n2) board
									 | otherwise = pawnBlackHelper (c1,n1) (c2,n2) board

pawnWhiteHelper (c1,n1) (c2,n2) board | c1 == c2 && n1 == 2 && n2 - n1 == 2 && boardSearch c2 (n1+1) board == "    |"  && boardSearch c2 n2 board == "    |" = True
									  | c1 == c2 && n2 - n1 == 1 && boardSearch c2 n2 board == "    |" = True
									  | n2 - n1 == 1 && (ord c2 == ord c1 - 1 || ord c2 == ord c1 + 1) && (elem (boardSearch c2 n2 board) [" PB |"," NB |"," QB |"," RB |"," BB |"]) = True
									  | otherwise = False

pawnBlackHelper (c1,n1) (c2,n2) board | c1 == c2 && n1 == 7 && n1 - n2 == 2 && boardSearch c2 (n1-1) board == "    |"  && boardSearch c2 n2 board == "    |"= True
									  | c1 == c2 && n1 - n2 == 1 && boardSearch c2 n2 board == "    |" = True
									  | n1 - n2 == 1 && (ord c2 == ord c1 - 1 || ord c2 == ord c1 + 1) && (elem (boardSearch c2 n2 board) [" PW |"," NW |"," QW |"," RW |"," BW |"]) = True
									  | otherwise = False

clearPathRook (c1,n1) (c2,n2) board | n2 > 8 || n2 < 1 || c2 > 'h' || c2 < 'a' = False
									| n1 < n2 && c1 == c2 = searchUp (c1,(n1+1)) (c2,n2) board
									| n1 > n2 && c1 == c2 = searchDown (c1,(n1-1)) (c2,n2) board
									| n1 == n2 && c1 < c2 = searchRight ((chr ((ord c1) + 1)),n1) (c2,n2) board
									| n1 == n2 && c1 > c2 = searchLeft ((chr ((ord c1) - 1)),n1) (c2,n2) board
									| otherwise = False

searchUp (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
							   | boardSearch c1 n1 board /= "    |" = False
							   | otherwise = searchUp (c1,(n1+1)) (c2,n2) board

searchDown (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
							     | boardSearch c1 n1 board /= "    |" = False
							     | otherwise = searchDown (c1,(n1-1)) (c2,n2) board

searchRight (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
								  | boardSearch c1 n1 board /= "    |" = False
							      | otherwise = searchRight ((chr ((ord c1) + 1)),n1) (c2,n2) board

searchLeft (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
								 | boardSearch c1 n1 board /= "    |" = False
							     | otherwise = searchLeft ((chr ((ord c1) - 1)),n1) (c2,n2) board

rookHelper (R (c1,n1)) board (c2,n2) = (clearPathRook (c1,n1) (c2,n2) board) && (checkTarget (R (c1,n1)) (c2,n2) board)

checkTarget piece (c2,n2) board | isWhite piece board == True && (elem (boardSearch c2 n2 board) [" PB |"," NB |"," QB |"," RB |"," BB |","    |"]) = True
								| isWhite piece board == False && (elem (boardSearch c2 n2 board) [" PW |"," NW |"," QW |"," RW |"," BW |","    |"]) = True
								| otherwise = False

clearPathBishop (c1,n1) (c2,n2) board | n2 > 8 || n2 < 1 || c2 > 'h' || c2 < 'a' = False
									  | (ord c2) - (ord c1) /= n2 - n1 && (ord c2) - (ord c1) /= n1 - n2 = False
									  | n1 < n2 && c1 < c2 = searchNorthEast ((chr ((ord c1) + 1)),(n1+1)) (c2,n2) board
									  | n1 > n2 && c1 < c2 = searchSouthEast ((chr ((ord c1) + 1)),(n1-1)) (c2,n2) board
									  | n1 < n2 && c1 > c2 = searchNorthWest ((chr ((ord c1) - 1)),(n1+1)) (c2,n2) board
									  | n1 > n2 && c1 > c2 = searchSouthWest ((chr ((ord c1) - 1)),(n1-1)) (c2,n2) board
									  | otherwise = False

searchNorthEast (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
									  | boardSearch c1 n1 board /= "    |" = False
									  | otherwise = searchNorthEast ((chr ((ord c1) + 1)),(n1+1)) (c2,n2) board

searchSouthEast (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
									  | boardSearch c1 n1 board /= "    |" = False
									  | otherwise = searchSouthEast ((chr ((ord c1) + 1)),(n1-1)) (c2,n2) board

searchNorthWest (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
									  | boardSearch c1 n1 board /= "    |" = False
							          | otherwise = searchNorthWest ((chr ((ord c1) - 1)),(n1+1)) (c2,n2) board

searchSouthWest (c1,n1) (c2,n2) board | c1 == c2 && n1 == n2 = True
									  | boardSearch c1 n1 board /= "    |" = False
							          | otherwise = searchSouthWest ((chr ((ord c1) - 1)),(n1-1)) (c2,n2) board

bishopHelper (B (c1,n1)) board (c2,n2) = (clearPathBishop (c1,n1) (c2,n2) board) && (checkTarget (B (c1,n1)) (c2,n2) board)

knightHelper (N (c1,n1)) board (c2,n2) | n2 > 8 || n2 < 1 || c2 > 'h' || c2 < 'a' = False
									   | checkTarget (N (c1,n1)) (c2,n2) board == False = False
									   | (n2 == n1 + 1 || n2 == n1 - 1) && (ord c2 == ord c1 + 2 || ord c2 == ord c1 - 2) = True
									   | (n2 == n1 + 2 || n2 == n1 - 2) && (ord c2 == ord c1 + 1 || ord c2 == ord c1 - 1) = True
									   | otherwise = False

kingHelper (K (c1,n1)) board (c2,n2) | n2 > 8 || n2 < 1 || c2 > 'h' || c2 < 'a' = False
									 | checkTarget (K (c1,n1)) (c2,n2) board == False = False
									 | ord c2 == ord c1 - 1 && (n2 == n1 - 1 || n2 == n1 || n2 == n1 + 1) = True
									 | ord c2 == ord c1 && (n2 == n1 - 1 || n2 == n1 + 1) = True
									 | ord c2 == ord c1 + 1 && (n2 == n1 - 1 || n2 == n1 || n2 == n1 + 1) = True
									 | otherwise = False

queenHelper (Q (c1,n1)) board (c2,n2) = (clearPathBishop (c1,n1) (c2,n2) board || clearPathRook (c1,n1) (c2,n2) board) && (checkTarget (Q (c1,n1)) (c2,n2) board)

isLegal :: Piece -> Board -> Location -> Bool
isLegal (P (c1,n1)) board (c2,n2) = pawnHelper (P (c1,n1)) board (c2,n2)
isLegal (N (c1,n1)) board (c2,n2) = knightHelper (N (c1,n1)) board (c2,n2)
isLegal (K (c1,n1)) board (c2,n2) = kingHelper (K (c1,n1)) board (c2,n2)
isLegal (Q (c1,n1)) board (c2,n2) = queenHelper (Q (c1,n1)) board (c2,n2)
isLegal (R (c1,n1)) board (c2,n2) = rookHelper (R (c1,n1)) board (c2,n2)
isLegal (B (c1,n1)) board (c2,n2) = bishopHelper (B (c1,n1)) board (c2,n2)

suggestMoveColumns piece board c i | i > 8 = []
								   | isLegal piece board (c,i) == True = (c,i):(suggestMoveColumns piece board c (i+1))
							       | otherwise = suggestMoveColumns piece board c (i+1)

suggestMoveHelper piece board c i | c > 'h' = []
								  | otherwise = (suggestMoveColumns piece board c i) ++ (suggestMoveHelper piece board (chr (ord c + 1)) i)

suggestMove :: Piece -> Board -> [Location]
suggestMove piece board = suggestMoveHelper piece board 'a' 1

move :: Piece -> Location -> Board -> Board
move piece (c,n) (player,l1,l2) | elem piece l1 == False && elem piece l2 == False = error (show piece ++ " is not on the board.")
								| player == Black && isWhite piece (player,l1,l2) = error "This is Black player's turn, White can't move."
								| player == White && isWhite piece (player,l1,l2) == False = error "This is White player's turn, Black can't move."
								| isLegal piece (player,l1,l2) (c,n) == False = error ("Illegal move for piece " ++ show piece)
								| player == White = (Black,(override piece (c,n) l1),(deletePiece (c,n) l2))
								| otherwise = (White,(deletePiece (c,n) l1),(override piece (c,n) l2))
								
override (P (c1,n1)) (c2,n2) (h:t) | h == P (c1,n1) = (P (c2,n2)):t
								   | otherwise = h:(override (P (c1,n1)) (c2,n2) t)
override (N (c1,n1)) (c2,n2) (h:t) | h == N (c1,n1) = (N (c2,n2)):t
								   | otherwise = h:(override (N (c1,n1)) (c2,n2) t)
override (K (c1,n1)) (c2,n2) (h:t) | h == K (c1,n1) = (K (c2,n2)):t
								   | otherwise = h:(override (K (c1,n1)) (c2,n2) t)
override (Q (c1,n1)) (c2,n2) (h:t) | h == Q (c1,n1) = (Q (c2,n2)):t
								   | otherwise = h:(override (Q (c1,n1)) (c2,n2) t)
override (R (c1,n1)) (c2,n2) (h:t) | h == R (c1,n1) = (R (c2,n2)):t
								   | otherwise = h:(override (R (c1,n1)) (c2,n2) t)
override (B (c1,n1)) (c2,n2) (h:t) | h == B (c1,n1) = (B (c2,n2)):t
								   | otherwise = h:(override (B (c1,n1)) (c2,n2) t)

deletePiece (c,n) l | elem (P (c,n)) l  == True = delete (P (c,n)) l
					| elem (N (c,n)) l  == True = delete (N (c,n)) l
					| elem (K (c,n)) l  == True = delete (K (c,n)) l
					| elem (Q (c,n)) l  == True = delete (Q (c,n)) l
					| elem (R (c,n)) l  == True = delete (R (c,n)) l
					| elem (B (c,n)) l  == True = delete (B (c,n)) l
					| otherwise = l