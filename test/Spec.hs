{-# LANGUAGE BinaryLiterals #-}

module Main where

import Cheese

import Data.Word (Word64)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "pawns" $ do
    describe "returns moves for" $ do
       it "starting position" $
         whitePawnMoves initialBoard `shouldBe` 0b0000000000000000000000000000000011111111111111110000000000000000

       it "staggered position" $ do
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬛ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ♙ ⬜ ⬜    ⬜ ⬛ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ♙ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬛ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ♙ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬛ ⬜ ⬜ ⬛ ⬜ ⬛ ⬜ ⬛
         -- ♙ ⬜ ⬜ ⬜ ⬜ ♙ ⬜ ♙    ⬜ ⬜ ⬜ ⬛ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ♙ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         let board = emptyBoard { whitePawns = 0b0000000000100000000000100000010000000000101000010000100000000000 }
             layer = 0b0010000000000010000001000000000010101001000010000000000000000000
         whitePawnMoves board `shouldBe` layer

       it "capture or move up (centered, isolated)" $ do
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ♟ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ♙ ♙ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ♙ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         let board = emptyBoard { whitePawns = 0b0000000000000000000000000000000000010000000000000000000000000000,
                                  blackPawns = 0b0000000000000000000000000000100000000000000000000000000000000000
                                }
             layer = 0b0000000000000000000000000001100000000000000000000000000000000000
         whitePawnMoves board `shouldBe` layer

       it "possible overflow" $ do
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ♟   ♙ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ♙ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜   ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
         let board = emptyBoard { whitePawns   = 0b0000000000000000000000000000000000000000000000010000000000000000,
                                  blackPawns   = 0b0000000000000000000000000000000010000000000000000000000000000000
                                }
             layer = 0b0000000000000000000000000000000000000001000000000000000000000000
         whitePawnMoves board `shouldBe` layer
