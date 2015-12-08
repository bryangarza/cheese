{-# LANGUAGE BinaryLiterals       #-}
{-# LANGUAGE OverloadedStrings    #-}

module Cheese where

-- Base imports.
import Data.Bits   (Bits
                   ,clearBit
                   ,complement
                   ,countLeadingZeros
                   ,countTrailingZeros
                   ,popCount
                   ,setBit
                   ,shiftL
                   ,shiftR
                   ,testBit
                   ,xor
                   ,(.&.)
                   ,(.|.))
import Data.Char   (intToDigit)
import Data.Foldable (foldMap)
import Data.List   (intercalate, intersperse)
import Data.Word   (Word64)
import Text.Printf (printf)

-- External imports.
import Data.List.Split (chunksOf)

-- | 8x8 square to represent one piece type's layer.
type BoardLayer = Word64

data PieceType = Pawn
               | Knight
               | Bishop
               | Rook
               | Queen
               | King

-- | Board representation (bitboard).
data Board = Board
    { whitePawns   :: BoardLayer
    , whiteKnights :: BoardLayer
    , whiteBishops :: BoardLayer
    , whiteRooks   :: BoardLayer
    , whiteQueens  :: BoardLayer
    , whiteKing    :: BoardLayer
    , blackPawns   :: BoardLayer
    , blackKnights :: BoardLayer
    , blackBishops :: BoardLayer
    , blackRooks   :: BoardLayer
    , blackQueens  :: BoardLayer
    , blackKing    :: BoardLayer
    } deriving (Eq)

setBoardLayer :: Color -> PieceType -> Board -> BoardLayer -> Int -> (Board, Int)
setBoardLayer White Pawn   b x n = (b { whitePawns = x }, n)
setBoardLayer White Knight b x n = (b { whiteKnights = x }, n)
setBoardLayer White Bishop b x n = (b { whiteBishops = x }, n)
setBoardLayer White Rook   b x n = (b { whiteRooks = x }, n)
setBoardLayer White Queen  b x n = (b { whiteQueens = x }, n)
setBoardLayer White King   b x n = (b { whiteKing = x }, n)
setBoardLayer Black Pawn   b x n = (b { blackPawns = x }, n)
setBoardLayer Black Knight b x n = (b { blackKnights = x }, n)
setBoardLayer Black Bishop b x n = (b { blackBishops = x }, n)
setBoardLayer Black Rook   b x n = (b { blackRooks = x }, n)
setBoardLayer Black Queen  b x n = (b { blackQueens = x }, n)
setBoardLayer Black King   b x n = (b { blackKing = x }, n)

getConstructor :: Color -> PieceType -> (Board -> BoardLayer)
getConstructor White Pawn   = whitePawns
getConstructor White Knight = whiteKnights
getConstructor White Bishop = whiteBishops
getConstructor White Rook   = whiteRooks
getConstructor White Queen  = whiteQueens
getConstructor White King   = whiteKing
getConstructor Black Pawn   = blackPawns
getConstructor Black Knight = blackKnights
getConstructor Black Bishop = blackBishops
getConstructor Black Rook   = blackRooks
getConstructor Black Queen  = blackQueens
getConstructor Black King   = blackKing

data Color = Black | White
           deriving (Eq)

data BoardState = BoardState
    { board :: Board
    , turn  :: Color
    } deriving (Eq)

-- | Board layers as a list instead of as data fields.
lsLayers :: Maybe Color -> Board -> [BoardLayer]
lsLayers (Just Black) (Board _ _ _ _ _ _ g h i j k l) = [g,h,i,j,k,l]
lsLayers (Just White) (Board a b c d e f _ _ _ _ _ _) = [a,b,c,d,e,f]
lsLayers Nothing      (Board a b c d e f g h i j k l) = [a,b,c,d,e,f,g,h,i,j,k,l]

lsLayerTexts = [ "White Pawns:   "
               , "White Knights: "
               , "White Biships: "
               , "White Rooks:   "
               , "White Queens:  "
               , "White King:    "
               , "Black Pawns:   "
               , "Black Knights: "
               , "Black Bishops: "
               , "Black Rooks:   "
               , "Black Queens:  "
               , "Black King:    " ]

-- | List each board layer in hexadecimal.
showHex :: Board -> String
showHex board = res
  where res = concat (zipWith fmt lsLayerTexts eachLayer)
        fmt desc pieceLayer = desc ++ printf "0x%08x\n" pieceLayer
        eachLayer = lsLayers Nothing board

-- | Board with no pieces on it.
emptyBoard :: Board
emptyBoard = Board 0 0 0 0 0 0 0 0 0 0 0 0

{-| Starting position, little endian rank-file mapping:
      56 57 58 59 60 61 62 63
      48 49 50 51 52 53 54 55
      40 41 42 43 44 45 46 47
      32 33 34 35 36 37 38 39
      24 25 26 27 28 29 30 31
      16 17 18 19 20 21 22 23
      08 09 10 11 12 13 14 15
      00 01 02 03 04 05 06 07
|-}
initialBoard :: Board
initialBoard = Board
    { whitePawns   = 0b0000000000000000000000000000000000000000000000001111111100000000
    , whiteKnights = 0b0000000000000000000000000000000000000000000000000000000000100100
    , whiteBishops = 0b0000000000000000000000000000000000000000000000000000000001000010
    , whiteRooks   = 0b0000000000000000000000000000000000000000000000000000000010000001
    , whiteQueens  = 0b0000000000000000000000000000000000000000000000000000000000001000
    , whiteKing    = 0b0000000000000000000000000000000000000000000000000000000000010000
    , blackPawns   = 0b0000000011111111000000000000000000000000000000000000000000000000
    , blackKnights = 0b0010010000000000000000000000000000000000000000000000000000000000
    , blackBishops = 0b0100001000000000000000000000000000000000000000000000000000000000
    , blackRooks   = 0b1000000100000000000000000000000000000000000000000000000000000000
    , blackQueens  = 0b0000100000000000000000000000000000000000000000000000000000000000
    , blackKing    = 0b0001000000000000000000000000000000000000000000000000000000000000
    }

-- | Symbol for empty square when printing out board.
emptySym :: Char
emptySym = '⬜'

-- | Convert a binary piece layer into True and False layer.
pieceBools :: BoardLayer -> [Bool]
pieceBools x = map (testBit x) [63,62..0]

-- | Convert a binary piece layer into a string using supplied Char as
-- representation.
layerStr :: BoardLayer -> Char -> String
layerStr xs c = xs'
  where xs'     = map replace (pieceBools xs)
        replace x = if x then c else emptySym

-- | Overlay one layer onto another.
overlay :: String -> String -> String
overlay xs ys = merged
  where merged = zipWith (curry pieceOrEmpty) xs ys
        pieceOrEmpty ('⬜', y) = y
        pieceOrEmpty (x, '⬜') = x
        pieceOrEmpty (_, _)   = emptySym

-- | Add column of spaces between each file.
formatForPrint :: String -> String
formatForPrint x  = (intercalate "\n" spacedOut) ++ "\n\n"
  where spacedOut = map (intersperse ' ') split
        split     = map reverse (chunksOf 8 x)

-- | Print a layer to stdout.
putLayer :: BoardLayer -> IO ()
putLayer xs = putStrLn $ formatForPrint (layerStr xs '⬛')

eachLetter :: String
-- eachLetter = "PNBRQKpnbrqk"
eachLetter = "♙♘♗♖♕♔♟♞♝♜♛♚"

{-|
  Print all the layers overlayed. Looks like:
    r b n q k n b r
    p p p p p p p p
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    P P P P P P P P
    R B N Q K N B R
-}
instance Show Board where
  show board = formatForPrint mergedAll
    where mergedAll  = foldr overlay initial xs
          initial    = layerStr 0 '⬜'
          xs         = zipWith layerStr eachLayer eachLetter
          eachLayer  = lsLayers Nothing board

-- | Rename infix `or` to word (exists in Data.Bits but is not exported).
bitwiseOr :: Bits a => a -> a -> a
bitwiseOr x y = x .|. y

-- | Find all empty squares on a given board.
-- ~(whitePawns|whiteKnights|...|blackKing)
emptySquares :: Maybe Color -> Board -> BoardLayer
emptySquares c b = complement (orFold (lsLayers c b))

-- | Print out board with empty squares marked, for debugging purposes.
printEmptySquares :: Maybe Color -> Board -> IO ()
printEmptySquares c b = putLayer (emptySquares c b)

maskRank :: Int -> BoardLayer
maskRank 3 = 0b0000000000000000000000000000000000000000111111110000000000000000
maskRank 6 = 0b0000000000000000111111110000000000000000000000000000000000000000

-- TODO: Promotion, en passant
-- | Pawns moves and attacks.
pawnMoves :: Color -> Int -> Board -> (BoardLayer, Int)
pawnMoves c sq b = (moves, sq)
  where (pawns, shift, toMask, c') = case c of
          Black -> (blackPawns b, shiftR, 6, White)
          White -> (whitePawns b, shiftL, 3, Black)
        empty     = emptySquares Nothing b
        baseMoves = shift pawns 8 .&. empty
        moves     = baseMoves .|. twoSpaces .|. attacks
        twoSpaces = two .&. empty
          where
            two       = shift masked 8
            masked    = baseMoves .&. (maskRank toMask)
        attacks  = enemyPieces .&. attacksLR
          where
            enemyPieces = orFold $ lsLayers (Just c') b
            attacksLR   = attacksL .|. attacksR
            attacksL    = shift maskA 7
            attacksR    = shift maskH 9
            maskA       = pawns .&. clearFile A
            maskH       = pawns .&. clearFile H
        -- enPassant =

-- | Rename infix `and` to word (exists in Data.Bits but is not exported).
bitwiseAnd :: Bits a => a -> a -> a
bitwiseAnd x y = x .&. y

-- | Moves where the destination square is empty.
movesToEmptySquares :: Maybe Color -> Board -> BoardLayer -> BoardLayer
movesToEmptySquares c b bl = bitwiseAnd (emptySquares c b) bl

-- | Possible files (columns) of the board.
data File = A | B | C | D | E | F | G | H

-- | Clear out piece positions where moving left/right would fall off the board.
clearFile :: File -> BoardLayer
clearFile H = 0b0111111101111111011111110111111101111111011111110111111101111111
clearFile G = 0b1011111110111111101111111011111110111111101111111011111110111111
clearFile B = 0b1111110111111101111111011111110111111101111111011111110111111101
clearFile A = 0b1111111011111110111111101111111011111110111111101111111011111110

-- | Because (.|.) is arity 2, use folds.
orFold :: [BoardLayer] -> BoardLayer
orFold = foldr bitwiseOr 0

-- | Because (.&.) is arity 2, use folds.
andFold :: [BoardLayer] -> BoardLayer
andFold = foldr bitwiseAnd 1

-- | Left and right shifts for sliding pieces King and Knight.
shiftLR :: [(BoardLayer,Int)] -> [(BoardLayer,Int)] -> BoardLayer
shiftLR l r = orFold (mapL ++ mapR)
  where shiftL' = uncurry shiftL
        mapL    = map shiftL' l
        shiftR' = uncurry shiftR
        mapR    = map shiftR' r

-- | Possible moves and attacks for a king piece.
-- | [north, east, northwest, northeast]
-- | [south, west, southeast, southwest]
kingMoves :: Color -> Int -> Board -> (BoardLayer, Int)
kingMoves color sq board = (valid, sq)
  where king  = case color of
          Black -> blackKing board
          White -> whiteKing board
        clipA = king .&. clearFile A
        clipH = king .&. clearFile H
        moves = shiftLR [(king,8), (clipH,1), (clipA,7), (clipH,9)]
                        [(king,8), (clipA,1), (clipH,7), (clipA,9)]
        valid = movesToEmptySquares (Just color) board moves

{-|
  Possible moves and attacks for a knight piece.
    8 ~ ~ ~ ~ ~ ~ ~ ~
    7 ~ ~ ~ ~ ~ ~ ~ ~
    6 ~ ~ 2 ~ 3 ~ ~ ~
    5 ~ 1 ~ ~ ~ 4 ~ ~
    4 ~ ~ ~ N ~ ~ ~ ~
    3 ~ 8 ~ ~ ~ 5 ~ ~
    2 ~ ~ 7 ~ 6 ~ ~ ~
    1 ~ ~ ~ ~ ~ ~ ~ ~
      A B C D E F G H
  Left shifts find postions 1, 4, 2, and 3.
  Right shifts find postions 5, 8, 6, and 7.
|-}
knightMoves :: Color -> Int -> Board -> (BoardLayer, Int)
knightMoves color src board = (valid, src)
  where knight = case color of
          Black -> blackKnights board
          White -> whiteKnights board
        clipA  = knight .&. clearFile A
        clipB  = knight .&. clearFile B
        clipAB = clipA .&. clipB
        clipG  = knight .&. clearFile G
        clipH  = knight .&. clearFile H
        clipGH = clipG .&. clipH
        moves  = shiftLR [(clipAB,6), (clipGH,10), (clipA,15), (clipH,17)]
                         [(clipGH,6), (clipAB,10), (clipH,15), (clipA,17)]
        valid  = movesToEmptySquares (Just color) board moves

-- https://chessprogramming.wikispaces.com/On+an+empty+Board#Line%20Attacks
pieceAt :: Int -> BoardLayer
pieceAt sq = shiftL 0x1 sq

rankMask :: Int -> BoardLayer
rankMask sq = shiftL 0xff (sq .&. 56)

fileMask :: Int -> BoardLayer
fileMask sq = shiftL 0x0101010101010101 (sq .&. 7)

diagonalMask :: Int -> BoardLayer
diagonalMask sq = shiftL (shiftR maindia sout) nort
  where maindia = (0x8040201008040201 :: Word64)
        diag = 8 * (sq .&. 7) - (sq .&. 56)
        nort = -diag .&. (shiftR diag 31)
        sout =  diag .&. (shiftR (-diag) 31)

antiDiagMask :: Int -> BoardLayer
antiDiagMask sq = shiftL (shiftR maindia sout) nort
  where maindia = (0x0102040810204080 :: Word64)
        diag = 56 - 8 * (sq .&. 7) - (sq .&. 56)
        nort = -diag .&. (shiftR diag 31)
        sout = diag .&. (shiftR (-diag) 31)

maskEx :: (Int -> BoardLayer) -> (Int -> BoardLayer)
maskEx f = \sq -> pieceAt sq `xor` (f sq)

rankMaskEx :: Int -> BoardLayer
rankMaskEx = maskEx rankMask

fileMaskEx :: Int -> BoardLayer
fileMaskEx = maskEx fileMask

diagonalMaskEx :: Int -> BoardLayer
diagonalMaskEx = maskEx diagonalMask

antiDiagMaskEx :: Int -> BoardLayer
antiDiagMaskEx = maskEx antiDiagMask

rookAttacks sq = rankMask sq .|. fileMask sq
bishopAttacks sq = diagonalMask sq .|. antiDiagMask sq
queenAttacks sq = rookAttacks sq .|. bishopAttacks sq

-- occupancy & filemask[d] = potential blockers
-- potential blockers - 2 * squarebit[d2] = difference
-- difference ^ occupancy = changed
-- changed & filemask = north attacks[d2]

xxx sq b = attacks
  where
    occupancy = orFold (lsLayers Nothing b)
    fmask = fileMask sq
    potBlockers = occupancy .&. fmask
    difference = potBlockers - (2 * (pieceAt sq))
    changed = difference `xor` occupancy
    attacks = changed .&. fmask

isolate = emptyBoard
    {
      -- whiteKnights = 0b000001000000000000000000000000000000000000000000000000000000000
      -- whiteKing    = 0b0000000000000000000000000000000000000000000000000000000000000000
      -- whitePawns   = 0b0000000000000000000100000100000100000000000000001100101100000000
      whitePawns   = 0b0000000000000000000000000000000000010000000000000000000000000000,
      blackPawns   = 0b0000000000000000000000000000100000000000000000000000000000000000
    }
isolate' = emptyBoard { whiteKnights = 0b0000000000000000001000000000000001000000000000000000000000000000 }
isolate'' = emptyBoard { whiteKnights = 0b0000000000000000000000000000000000000000000000000000000000000001 }
isolate''' = emptyBoard
    {
      whitePawns   = 0b0000000000000000000000000000000000000000000000010000000000000000,
      blackPawns   = 0b0000000000000000000000000000000000000000100000000000000000000000
    }

-- . . . . . . . . . . . . . . . .
-- . . . . . . . . . . . . . . . .
-- . . . . . . . . . x . . . . . .
-- . p . . . . . . . . . . x . . .
-- . . . . p . . . x x x x . x x .
-- . . p . . . . . x x . x . x x .
-- p p . p . p p . . . . . . . . .
-- . . . . . . . . . . . . . . . .

{-| Starting position, little endian rank-file mapping:
      56 57 58 59 60 61 62 63
      48 49 50 51 52 53 54 55
      40 41 42 43 44 45 46 47
      32 33 34 35 36 37 38 39
      24 25 26 27 28 29 30 31
      16 17 18 19 20 21 22 23
      08 09 10 11 12 13 14 15
      00 01 02 03 04 05 06 07
|-}
-- occupancy = 0b0010100001100101100010100010000000001010010000001010101101011000
occupancy :: Board
occupancy = emptyBoard {
  blackPawns = 0b0000000010010010010001000100010000000000000000000000000000000000,
  whitePawns = 0b0000000000000000000000000000000000000001010010110000100010100000
  }

rookMoves :: Color -> Int -> Board -> (BoardLayer, Int)
rookMoves c sq b  = ((rank .|. file)  .&. sameColor, sq)
  where
    sameColor = complement (orFold $ lsLayers (Just c) b)
    occupancy = orFold (lsLayers Nothing b)
    file = (2 * ls1bUpper - ms1bLower) .&. fileMaskEx sq
      where
        occupancyFile  = occupancy .&. fileMaskEx sq
        sqSucc         = sq + 1
        occupancyUpper = occupancyFile .&. (complement $ (shiftL 1 sqSucc) - 1)
        occupancyLower = occupancyFile .&. ((shiftL 1 sqSucc) - 1)
        ls1bUpper      = pieceAt (countTrailingZeros occupancyUpper)
        ms1bLower      = pieceAt $ (63 - (countLeadingZeros lower))
        lower          = occupancyLower .|. 0b1
    rank = (2 * ls1bUpper - ms1bLower) .&. rankMaskEx sq
      where
        occupancyRank  = occupancy .&. rankMaskEx sq
        sqSucc         = sq + 1
        occupancyUpper = occupancyRank .&. (complement $ (shiftL 1 sqSucc) - 1)
        occupancyLower = occupancyRank .&. ((shiftL 1 sqSucc) - 1)
        ls1bUpper      = pieceAt (countTrailingZeros occupancyUpper)
        ms1bLower      = pieceAt $ (63 - (countLeadingZeros lower))
        lower          = occupancyLower .|. 0b1

bishopMoves :: Color -> Int -> Board -> (BoardLayer, Int)
bishopMoves c sq b  = ((rank .|. file)  .&. sameColor, sq)
  where
    sameColor = complement (orFold $ lsLayers (Just c) b)
    occupancy = orFold (lsLayers Nothing b)
    file = (2 * ls1bUpper - ms1bLower) .&. diagonalMaskEx sq
      where
        occupancyFile  = occupancy .&. diagonalMaskEx sq
        sqSucc         = sq + 1
        occupancyUpper = occupancyFile .&. (complement $ (shiftL 1 sqSucc) - 1)
        occupancyLower = occupancyFile .&. ((shiftL 1 sqSucc) - 1)
        ls1bUpper      = pieceAt (countTrailingZeros occupancyUpper)
        ms1bLower      = pieceAt $ (63 - (countLeadingZeros lower))
        lower          = occupancyLower .|. 0b1
    rank = (2 * ls1bUpper - ms1bLower) .&. antiDiagMaskEx sq
      where
        occupancyRank  = occupancy .&. antiDiagMaskEx sq
        sqSucc         = sq + 1
        occupancyUpper = occupancyRank .&. (complement $ (shiftL 1 sqSucc) - 1)
        occupancyLower = occupancyRank .&. ((shiftL 1 sqSucc) - 1)
        ls1bUpper      = pieceAt (countTrailingZeros occupancyUpper)
        ms1bLower      = pieceAt $ (63 - (countLeadingZeros lower))
        lower          = occupancyLower .|. 0b1

queenMoves :: Color -> Int -> Board -> (BoardLayer, Int)
queenMoves c sq b = (rook .|. bishop, sq)
  where (rook,_)   = (rookMoves c sq b)
        (bishop,_) = (bishopMoves c sq b)

-- ex: movePiece White Queen 3 12 whiteQueens firstMove
movePiece :: Color
          -> PieceType
          -> Int
          -> Int
          -> Board
          -> Board
movePiece color pieceType sq sq' b = res
  where
    (res,_) = setBoardLayer color pieceType b newLayer sq
    remove = clearBit ((getConstructor color pieceType) b) sq
    newLayer = setBit remove sq'

eachBit :: BoardLayer -> [Int]
eachBit layer = go layer [] (popCount layer)
  where go l res 0 = res
        go l res n = go (clearBit l sq) (sq : res) (pred n)
          where sq = countTrailingZeros l

getMovesFunc
  :: PieceType -> Color -> Int -> Board -> (BoardLayer, Int)
getMovesFunc Pawn = pawnMoves
getMovesFunc Knight = knightMoves
getMovesFunc Bishop = bishopMoves
getMovesFunc Rook = rookMoves
getMovesFunc Queen = queenMoves
getMovesFunc King = kingMoves

eachMove :: Color -> PieceType -> Board -> BoardLayer -> Int -> [Board]
eachMove c p b moves src = map (\dest -> movePiece c p src dest b) (eachBit moves)

pieceMovesEach :: Color -> PieceType -> Board -> [Board]
pieceMovesEach c t b =
  foldMap (eachMove' . someMove . separatePieces) eachBit'
  where eachBit'                       = eachBit ((getConstructor c t) b)
        separatePieces sq              = setBoardLayer c t b (setBit 0b0 sq) sq
        someMove (onePiece, src)       = (getMovesFunc t) c src onePiece
        eachMove' (isolatedBoard, src) = eachMove c t b isolatedBoard src

-- allMoves :: BoardState -> [BoardState]
-- allMoves bs =
--   color = turn bs

-- testRound :: Color -> Board -> BoardLayer
-- testRound c b = orFold [(pawnMoves c b)
--                        ,(knightMoves c b)
--                        ,(kingMoves c b)
--                        ,(rookMoves c 56 b)
--                        ,(rookMoves c 63 b)
--                        ,(bishopMoves c 57 b)
--                        ,(bishopMoves c 62 b)
--                        ,(queenMoves c 59 b)]
                -- break here
                       -- ,(rookMoves c 0 b)
                       -- ,(rookMoves c 7 b)
                       -- ,(bishopMoves c 1 b)
                       -- ,(bishopMoves c 6 b)
                       -- ,(queenMoves c 3 b)]
firstMove = initialBoard { whitePawns = 0b0000000000000000000000000000000000000000000100001110111100000000 }
bish = initialBoard { whiteBishops =    0b0000000000000000000000000010000000001000000000000000000000000000 }
