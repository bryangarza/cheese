{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Cheese where

-- Base imports.
import Data.Bits   (Bits, complement, shiftL, shiftR, testBit, (.&.), (.|.))
import Data.Char   (intToDigit)
import Data.List   (intercalate, intersperse)
import Data.Word   (Word64)
import Text.Printf (printf)

-- External imports.
import Data.List.Split (chunksOf)

-- | 8x8 square to represent one piece type's layer.
type BoardLayer = Word64

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

data Color = Black | White

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

-- | Starting position.
initialBoard :: Board
initialBoard = Board
    { whitePawns   = 0b0000000000000000000000000000000000000000000000001111111100000000
    , whiteKnights = 0b0000000000000000000000000000000000000000000000000000000000100100
    , whiteBishops = 0b0000000000000000000000000000000000000000000000000000000001000010
    , whiteRooks   = 0b0000000000000000000000000000000000000000000000000000000010000001
    , whiteQueens  = 0b0000000000000000000000000000000000000000000000000000000000010000
    , whiteKing    = 0b0000000000000000000000000000000000000000000000000000000000001000
    , blackPawns   = 0b0000000011111111000000000000000000000000000000000000000000000000
    , blackKnights = 0b0010010000000000000000000000000000000000000000000000000000000000
    , blackBishops = 0b0100001000000000000000000000000000000000000000000000000000000000
    , blackRooks   = 0b1000000100000000000000000000000000000000000000000000000000000000
    , blackQueens  = 0b0001000000000000000000000000000000000000000000000000000000000000
    , blackKing    = 0b0000100000000000000000000000000000000000000000000000000000000000
    }

-- | Symbol for empty square when printing out board.
emptySym :: Char
emptySym = '.'

-- | Convert a binary piece layer into True and False layer.
pieceBools :: BoardLayer -> [Bool]
pieceBools x = pb x 63
  where pb _ (-1)  = []
        pb x index = testBit x index : pb x (pred index)

-- | Convert a binary piece layer into a string using supplied Char as representation.
layer :: BoardLayer -> Char -> String
layer xs c = xs'
  where xs'     = map replace (pieceBools xs)
        replace x = if x then c else emptySym

-- | Overlay one layer onto another.
overlay :: String -> String -> String
overlay xs ys = merged
  where merged = zipWith (curry pieceOrEmpty) xs ys
        pieceOrEmpty ('.', y) = y
        pieceOrEmpty (x, '.') = x
        pieceOrEmpty (_, _)   = emptySym

-- | Add column of spaces between each file.
formatForPrint :: String -> String
formatForPrint x  = intercalate "\n" spacedOut
  where spacedOut = map (intersperse ' ') split
        split     = chunksOf 8 x

-- | Print a layer to stdout.
putLayer :: BoardLayer -> IO ()
putLayer xs = putStrLn $ formatForPrint (layer xs 'x')

eachLetter :: String
eachLetter = "pnbrqkPNBRQK"

{-|
  Print all the layers overlayed. Looks like:
    R B N Q K N B R
    P P P P P P P P
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    p p p p p p p p
    r b n q k n b r
-}
instance Show Board where
  show board = formatForPrint mergedAll
    where mergedAll  = foldr overlay initial xs
          initial    = layer 0 '.'
          xs         = zipWith layer eachLayer eachLetter
          eachLayer  = lsLayers Nothing board

-- | Rename infix `or` to word (exists in Data.Bits but is not exported).
bitwiseOr :: Bits a => a -> a -> a
bitwiseOr x y = x .|. y

-- | Find all empty squares on a given board.
-- ~(whitePawns|whiteKnights|...|blackKing)
emptySquares :: Maybe Color -> Board -> BoardLayer
emptySquares c b = complement (orFold $ lsLayers c b)

-- | Print out board with empty squares marked, for debugging purposes.
printEmptySquares :: Maybe Color -> Board -> IO ()
printEmptySquares c b = putLayer (emptySquares c b)

-- TODO: Promotion, en passant, two-square initial move rule...
-- | All white pawns moved up one square.
whitePawnMoves :: Board -> BoardLayer
whitePawnMoves = flip shiftL 8 . whitePawns

-- | All black pawns moved up one square.
blackPawnMoves :: Board -> BoardLayer
blackPawnMoves = flip shiftR 8 . blackPawns

-- | Rename infix `and` to word (exists in Data.Bits but is not exported).
bitwiseAnd :: Bits a => a -> a -> a
bitwiseAnd x y = x .&. y

-- | Moves where the destination square is empty.
movesToEmptySquares :: Maybe Color -> Board -> BoardLayer -> BoardLayer
movesToEmptySquares c b = bitwiseAnd (emptySquares c b)

-- | Possible files (columns) of the board.
data File = A | B | C | D | E | F | G | H

-- | Clear out piece positions where moving left/right would fall off the board.
clearFile :: File -> BoardLayer
clearFile x = case x of
  A -> 0b0111111101111111011111110111111101111111011111110111111101111111
  B -> 0b1011111110111111101111111011111110111111101111111011111110111111
  G -> 0b1111110111111101111111011111110111111101111111011111110111111101
  H -> 0b1111111011111110111111101111111011111110111111101111111011111110

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
-- | [north, west, northeast, northwest]
-- | [south, east, southwest, southeast]
kingMoves :: Color -> Board -> BoardLayer
kingMoves color board = valid
  where king  = case color of
          Black -> blackKing board
          White -> whiteKing board
        clipA = king .&. clearFile A
        clipH = king .&. clearFile H
        moves = shiftLR [(king,8), (clipA,1), (clipH,7), (clipA,9)]
                        [(king,8), (clipH,1), (clipA,7), (clipH,9)]
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
  Left shifts find postions 4, 1, 3, and 2.
  Right shifts find postions 8, 5, 7, and 6.
|-}
knightMoves :: Color -> Board -> BoardLayer
knightMoves color board = valid
  where knight = case color of
          Black -> blackKnights board
          White -> whiteKnights board
        clipA  = knight .&. clearFile A
        clipB  = knight .&. clearFile B
        clipAB = clipA .&. clipB
        clipG  = knight .&. clearFile G
        clipH  = knight .&. clearFile H
        clipGH = clipG .&. clipH
        moves  = shiftLR [(clipGH,6), (clipAB,10), (clipG,15), (clipA,17)]
                         [(clipAB,6), (clipGH,10), (clipA,15), (clipG,17)]
        valid  = movesToEmptySquares (Just color) board moves
