{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BinaryLiterals #-}
module Cheese where

import Data.Char (intToDigit)
import Data.Word
import Text.Printf
import Data.Bits
import Data.List (intercalate, intersperse)
import Data.List.Split

type BoardLayer = Word64

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

-- | Print each board layer in hexadecimal.
instance Show Board where
  show (Board wp wn wb wr wq wk bp bn bb br bq bk) =
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

emptyBoard :: Board
emptyBoard = Board 0 0 0 0 0 0 0 0 0 0 0 0

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

emptySym :: Char
emptySym = '.'

-- | Convert a binary piece layer into True and False layer.
pieceBools :: BoardLayer -> [Bool]
pieceBools x = thing' x 63
  where thing' _ (-1)  = []
        thing' x index = testBit x index : (thing' x (pred index))

-- | Convert a binary piece layer into a string using supplied Char as representation.
layer :: BoardLayer -> Char -> String
layer xs c = xs'
  where xs'     = map replace (pieceBools xs)
        replace = \x -> if x then c else emptySym

-- | Print a layer to stdout.
putLayer :: BoardLayer -> Char -> IO ()
putLayer xs c = putStrLn (layer xs c)

-- | Overlay one layer onto another.
overlay :: String -> String -> String
overlay xs ys = merged
  where merged = map pieceOrEmpty (zip xs ys)
        pieceOrEmpty ('.', y) = y
        pieceOrEmpty (x, '.') = x
        pieceOrEmpty (_, _)   = emptySym

{-|
  Print all the layers overlayed. Look like:
    R B N Q K N B R
    P P P P P P P P
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    . . . . . . . .
    p p p p p p p p
    r b n q k n b r
-}
overlayedLayers = intercalate "\n" spacedOut
  where spacedOut  = map (intersperse ' ') split
        split      = chunksOf 8 mergedAll
        mergedAll  = foldr overlay (layer (0 :: BoardLayer) '.') xs
        xs = map layer' [ ((whitePawns initialBoard),   'p')
                        , ((whiteKnights initialBoard), 'n')
                        , ((whiteBishops initialBoard), 'b')
                        , ((whiteRooks initialBoard),   'r')
                        , ((whiteQueens initialBoard),  'q')
                        , ((whiteKing initialBoard),    'k')
                        , ((blackPawns initialBoard),   'P')
                        , ((blackKnights initialBoard), 'N')
                        , ((blackBishops initialBoard), 'B')
                        , ((blackRooks initialBoard),   'R')
                        , ((blackQueens initialBoard),  'Q')
                        , ((blackKing initialBoard),    'K')
                        ]
        layer' (piece, letter) = layer piece letter
