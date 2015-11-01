{-# LANGUAGE BinaryLiterals #-}

module Main where

import Cheese

import Data.Word (Word64)
import Test.Hspec

absolute :: Int -> Int
absolute = undefined

main :: IO ()
main = hspec $ do
  describe "pawns" $ do
    it "returns moves for starting position" $
      whitePawnMoves initialBoard `shouldBe` 0b0000000000000000000000000000000011111111111111110000000000000000

    it "returns moves for staggered position" $
      -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬛ ⬜ ⬜
      -- ⬜ ⬜ ⬜ ⬜ ⬜ ♙ ⬜ ⬜    ⬜ ⬛ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
      -- ⬜ ♙ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬛ ⬜ ⬜ ⬜ ⬜ ⬜
      -- ⬜ ⬜ ♙ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
      -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬛ ⬜ ⬜ ⬛ ⬜ ⬛ ⬜ ⬛
      -- ♙ ⬜ ⬜ ⬜ ⬜ ♙ ⬜ ♙    ⬜ ⬜ ⬜ ⬛ ⬜ ⬜ ⬜ ⬜
      -- ⬜ ⬜ ⬜ ♙ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
      -- ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜    ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜ ⬜
      whitePawnMoves board `shouldBe` layer
        where board = emptyBoard { whitePawns = 0b0000000000100000000000100000010000000000101000010000100000000000 }
              layer = 0b0010000000000010000001000000000010101001000010000000000000000000
