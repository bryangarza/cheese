{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Cheese where

-- Base imports.
import Data.Bits       (Bits, complement, testBit, (.|.))
import Data.Char       (intToDigit)
import Data.List       (intercalate, intersperse)
import Data.Word       (Word64)
import Text.Printf     (printf)

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

-- | List each board layer in hexadecimal.
showHex :: Board -> String
showHex (Board wp wn wb wr wq wk bp bn bb br bq bk) =
  concat [ "White Pawns:   " ++ fmt wp
         , "White Knights: " ++ fmt wn
         , "White Biships: " ++ fmt wp
         , "White Rooks:   " ++ fmt wr
         , "White Queens:  " ++ fmt wq
         , "White King:    " ++ fmt wk
         , "Black Pawns:   " ++ fmt bp
         , "Black Knights: " ++ fmt bb
         , "Black Bishops: " ++ fmt bb
         , "Black Rooks:   " ++ fmt br
         , "Black Queens:  " ++ fmt bq
         , "Black King:    " ++ fmt' bk ]
  where fmt  = printf "0x%08x\n"
        fmt' = printf "0x%08x"

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
pieceBools x = thing' x 63
  where thing' _ (-1)  = []
        thing' x index = testBit x index : thing' x (pred index)

-- | Convert a binary piece layer into a string using supplied Char as representation.
layer :: BoardLayer -> Char -> String
layer xs c = xs'
  where xs'     = map replace (pieceBools xs)
        replace = \x -> if x then c else emptySym

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
    where mergedAll  = foldr overlay (layer (0 :: BoardLayer) '.') xs
          xs = map layer' [ (whitePawns board,   'p')
                          , (whiteKnights board, 'n')
                          , (whiteBishops board, 'b')
                          , (whiteRooks board,   'r')
                          , (whiteQueens board,  'q')
                          , (whiteKing board,    'k')
                          , (blackPawns board,   'P')
                          , (blackKnights board, 'N')
                          , (blackBishops board, 'B')
                          , (blackRooks board,   'R')
                          , (blackQueens board,  'Q')
                          , (blackKing board,    'K')
                          ]
          layer' (piece, letter) = layer piece letter

-- | Board layers as a list instead of as data fields.
lsLayers :: Board -> [BoardLayer]
lsLayers (Board a b c d e f g h i j k l) = [a,b,c,d,e,f,g,h,i,j,k,l]

-- | Rename infix `or` to word (exists in Data.Bits but is not exported).
bitwiseOr :: Bits a => a -> a -> a
bitwiseOr x y = x .|. y

-- | Find all empty squares on a given board.
-- ~(whitePawns|whiteKnights|...|blackKing)
emptySquares :: Board -> BoardLayer
emptySquares = complement . foldr bitwiseOr (0 :: BoardLayer) . lsLayers

-- | Print out board with empty squares marked, for debugging purposes.
printEmptySquares :: Board -> IO ()
printEmptySquares board = putLayer (emptySquares board)
