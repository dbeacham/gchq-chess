module Main where

import Prelude
import Data.Enum
import Data.Maybe
import Data.Ord
-- import Control.Monad.Eff
-- import Control.Monad.Eff.Console
-- 
-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do
--   log "Hello sailor!"

data Colour
  = White
  | Black

data Piece
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King

instance eqPiece :: Eq Piece where
  eq Pawn   Pawn   = true
  eq Knight Knight = true
  eq Bishop Bishop = true
  eq Rook   Rook   = true
  eq Queen  Queen  = true
  eq King   King   = true
  eq _      _      = false

instance showPiece :: Show Piece where
  show Pawn   = "Pawn"
  show Knight = "Knight"
  show Bishop = "Bishop"
  show Rook   = "Rook"
  show Queen  = "Queen"
  show King   = "King"

instance boundedPiece :: Bounded Piece where
  bottom = Pawn
  top = King

instance enumPiece :: Enum Piece where
  cardinality = Cardinality 6
  succ = defaultSucc pieceToEnum pieceFromEnum
  pred = defaultPred pieceToEnum pieceFromEnum
  toEnum = pieceToEnum
  fromEnum = pieceFromEnum
        
pieceToEnum :: Int -> Maybe Piece
pieceToEnum 0 = Just Pawn
pieceToEnum 1 = Just Knight
pieceToEnum 2 = Just Bishop
pieceToEnum 3 = Just Rook
pieceToEnum 4 = Just Queen
pieceToEnum 5 = Just King
pieceToEnum _ = Nothing

pieceFromEnum :: Piece -> Int
pieceFromEnum Pawn   = 0
pieceFromEnum Knight = 1
pieceFromEnum Bishop = 2
pieceFromEnum Rook   = 3
pieceFromEnum Queen  = 4
pieceFromEnum King   = 5

instance ordPiece :: Ord Piece where
  compare = comparing pieceFromEnum
