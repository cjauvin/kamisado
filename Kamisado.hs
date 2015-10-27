{-# LANGUAGE OverloadedStrings, ViewPatterns, PatternSynonyms, DeriveGeneric, DeriveAnyClass #-}

module Kamisado where

import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Data.Aeson
import Data.Char
import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Exception (assert)
import GHC.Generics

fromList = Sequence.fromList
index = Sequence.index
update = Sequence.update

-- http://stackoverflow.com/questions/31106484/pattern-matching-data-sequence-like-lists
pattern Empty   <- (Sequence.viewl -> Sequence.EmptyL)  where Empty = Sequence.empty
pattern x :< xs <- (Sequence.viewl -> x Sequence.:< xs) where (:<)  = (Sequence.<|)
pattern xs :> x <- (Sequence.viewr -> xs Sequence.:> x) where (:>)  = (Sequence.|>)

data KColor = KOrange |
              KBlue |
              KPurple |
              KPink |
              KYellow |
              KRed |
              KGreen |
              KBrown
             deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- instance FromJSON KColor
-- instance ToJSON KColor where
--   -- http://stackoverflow.com/a/33046010/787842
--   toJSON = genericToJSON defaultOptions

data Player = KWhite |
              KBlack
             deriving (Eq, Show, Generic, ToJSON, FromJSON)

getPlayerStr :: Maybe Player -> String
getPlayerStr (Just KWhite) = "X"
getPlayerStr (Just KBlack) = "O"
getPlayerStr Nothing = " "

data Piece = Piece {
    getPlayer :: Player,
    getColor :: KColor
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Seq = Sequence.Seq
type Cell = (KColor, Maybe Piece)
type Board = Seq (Seq (Cell))
type Coord = (Int, Int)

inf = (read "Infinity")::Double

-- 4 X 4
-- initBoard :: Board
-- initBoard = fromList [
--   fromList [(KOrange, Just $ Piece KBlack KOrange), (KGreen, Just $ Piece KBlack KGreen),
--             (KBlue, Just $ Piece KBlack KBlue), (KYellow, Just $ Piece KBlack KYellow)],
--   fromList [(KYellow, Nothing), (KOrange, Nothing), (KGreen, Nothing), (KBlue, Nothing)],
--   fromList [(KBlue, Nothing), (KYellow, Nothing), (KOrange, Nothing), (KGreen, Nothing)],
--   fromList [(KGreen, Just $ Piece KWhite KGreen), (KBlue, Just $ Piece KWhite KBlue),
--             (KYellow, Just $ Piece KWhite KYellow), (KOrange, Just $ Piece KWhite KOrange)]]

initBoard :: Board
initBoard = fromList [
  fromList [(KOrange, Just $ Piece KBlack KOrange), (KBlue, Just $ Piece KBlack KBlue),
            (KPurple, Just $ Piece KBlack KPurple), (KPink, Just $ Piece KBlack KPink),
            (KYellow, Just $ Piece KBlack KYellow), (KRed, Just $ Piece KBlack KRed),
            (KGreen, Just $ Piece KBlack KGreen), (KBrown, Just $ Piece KBlack KBrown)],
  fromList [(KRed, Nothing), (KOrange, Nothing), (KPink, Nothing), (KGreen, Nothing),
            (KBlue, Nothing), (KYellow, Nothing), (KBrown, Nothing), (KPurple, Nothing)],
  fromList [(KGreen, Nothing), (KPink, Nothing), (KOrange, Nothing), (KRed, Nothing),
            (KPurple, Nothing), (KBrown, Nothing), (KYellow, Nothing), (KBlue, Nothing)],
  fromList [(KPink, Nothing), (KPurple, Nothing), (KBlue, Nothing), (KOrange, Nothing),
            (KBrown, Nothing), (KGreen, Nothing), (KRed, Nothing), (KYellow, Nothing)],
  fromList [(KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing), (KBrown, Nothing),
            (KOrange, Nothing), (KBlue, Nothing), (KPurple, Nothing), (KPink, Nothing)],
  fromList [(KBlue, Nothing), (KYellow, Nothing), (KBrown, Nothing), (KPurple, Nothing),
            (KRed, Nothing), (KOrange, Nothing), (KPink, Nothing), (KGreen, Nothing)],
  fromList [(KPurple, Nothing), (KBrown, Nothing), (KYellow, Nothing), (KBlue, Nothing),
            (KGreen, Nothing), (KPink, Nothing), (KOrange, Nothing), (KRed, Nothing)],
  fromList [(KBrown, Just $ Piece KWhite KBrown), (KGreen, Just $ Piece KWhite KGreen),
            (KRed, Just $ Piece KWhite KRed), (KYellow, Just $ Piece KWhite KYellow),
            (KPink, Just $ Piece KWhite KPink), (KPurple, Just $ Piece KWhite KPurple),
            (KBlue, Just $ Piece KWhite KBlue), (KOrange, Just $ Piece KWhite KOrange)]]

gridSize = length initBoard

cellAt :: Board -> Coord -> Cell
cellAt board (i, j) = index (index board i) j
cellColorAt :: Board -> Coord -> KColor
cellColorAt board (i, j) = fst (cellAt board (i, j))
pieceAt :: Board -> Coord -> Maybe Piece
pieceAt board (i, j) = snd (cellAt board (i, j))
pieceColorAt :: Board -> Coord -> Maybe KColor
-- This is equivalent to: fmap getColor $ pieceAt board (i, j)
pieceColorAt board (i, j) = getColor <$> pieceAt board (i, j)
playerAt :: Board -> Coord -> Maybe Player
playerAt board (i, j) = getPlayer <$> pieceAt board (i, j)
cellEmpty :: Board -> Coord -> Bool
cellEmpty board (i, j) = pieceAt board (i, j) == Nothing

upward :: Coord -> [Coord]
upward (i, j) = [(a, b) | a <- [i - 1, i - 2..0], b <- [j]]
downward :: Coord -> [Coord]
downward (i, j) = [(a, b) | a <- [i + 1..gridSize-1], b <- [j]]
upleftward :: Coord -> [Coord]
upleftward (i, j) = [(i - k, j - k) | k <- [1..gridSize], i - k >= 0, j - k >= 0]
uprightward :: Coord -> [Coord]
uprightward (i, j) = [(i - k, j + k) | k <- [1..gridSize], i - k >= 0, j + k < gridSize]
downleftward :: Coord -> [Coord]
downleftward (i, j) = [(i + k, j - k) | k <- [1..gridSize], i + k < gridSize, j - k >= 0]
downrightward :: Coord -> [Coord]
downrightward (i, j) = [(i + k, j + k) | k <- [1..gridSize], i + k < gridSize, j + k < gridSize]

updateBoard :: Board -> Coord -> Coord -> Board
updateBoard board (i0, j0) (i1, j1) =
  assert (pieceAt board (i0, j0) /= Nothing)
  assert (pieceAt board (i1, j1) == Nothing)
  update i0 srcRow $ dstBoard
  where srcCell = (cellColorAt board (i0, j0), Nothing)
        srcRow = update j0 srcCell $ index board i0
        dstCell = (cellColorAt board (i1, j1), pieceAt board (i0, j0))
        dstRow = update j1 dstCell $ index board i1
        dstBoard = update i1 dstRow $ board

getPlayerPieceCoords :: Board -> Player -> [Coord]
getPlayerPieceCoords board player =
  filter (\(i, j) -> (playerAt board (i, j)) == Just player) coords
  where coords = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

getPlayerPieceColorCoord :: Board -> Player -> KColor -> Coord
getPlayerPieceColorCoord board player color =
  fromJust $ find (\(i, j) -> (pieceAt board (i, j)) == (Just $ Piece player color)) coords
  where coords = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

getPossibleCoordsFromCoord :: Board -> Coord -> [Coord]
getPossibleCoordsFromCoord board (i, j) =
  case player of
    KWhite -> concat $ map constrainMoves
             [upleftward (i, j), upward (i, j), uprightward (i, j)]
    KBlack -> concat $ map constrainMoves
             [downleftward (i, j), downward (i, j), downrightward (i, j)]
  where constrainMoves = takeWhile $ cellEmpty board
        player = fromJust $ playerAt board (i, j)

isWinning :: Board -> Player -> Bool
isWinning board player =
  any (Just player ==) playersInWinningRow
  where playersInWinningRow = (fmap getPlayer) <$> playerWinningRow
        playerWinningRow = snd <$> index board rowIdx
        rowIdx = case player of
          KWhite -> 0
          KBlack -> gridSize - 1

pieceAtCoordCanWinInOne :: Board -> Coord -> Bool
pieceAtCoordCanWinInOne board (i, j) =
  not $ null $ filter (\(i, _) -> i == playerWinningRowIdx) moves
  where player = fromJust $ playerAt board (i, j)
        moves = getPossibleCoordsFromCoord board (i, j)
        playerWinningRowIdx = case player of
          KWhite -> 0
          KBlack -> gridSize - 1

getDistinctColorsForPieceAtCoord :: Board -> Coord -> Set.Set KColor
getDistinctColorsForPieceAtCoord board src =
  Set.fromList $ map (cellColorAt board) dstCoords
  where dstCoords = getPossibleCoordsFromCoord board src

boardValueForPlayer :: Board -> Player -> KColor -> Double
boardValueForPlayer board player color =
  -- trace (show (winning,
  --              (fromIntegral nbWinInOnePlayerPieces),
  --              losing,
  --              (fromIntegral nbWinInOneOpponentPieces),
  --              (fromIntegral nbDistinctColorsForNextOpponentMove),
  --              pos - neg))
  pos - neg
  where opponent = if player == KWhite then KBlack else KWhite
        winning = if isWinning board player then inf else 0
        losing =  if isWinning board opponent then inf else 0
        nbWinInOnePlayerPieces = length $ filter (\(i, j) ->
                                                    pieceAtCoordCanWinInOne
                                                    board (i, j)) playerPieceCoords
        playerPieceCoords = getPlayerPieceCoords board player
        nbWinInOneOpponentPieces = length $ filter (\(i, j) ->
                                                      pieceAtCoordCanWinInOne
                                                      board (i, j)) opponentPieceCoords
        opponentPieceCoord = getPlayerPieceColorCoord board opponent color
        nbDistinctColorsForNextOpponentMove = length $ getDistinctColorsForPieceAtCoord
                                                       board opponentPieceCoord
        opponentPieceCoords = getPlayerPieceCoords board opponent
        pos = winning + (fromIntegral nbWinInOnePlayerPieces)
        neg = losing + (fromIntegral nbWinInOneOpponentPieces) +
              (fromIntegral nbDistinctColorsForNextOpponentMove)

findBestMoveCoord :: Board -> Player -> KColor -> Int -> Maybe Coord
findBestMoveCoord board player color depth =
  assert (isLegalMove board player (Just color) srcCoord bestDstCoord)
  --trace (show valueDstCoordPairs)
  bestDstCoord
  where srcCoord = getPlayerPieceColorCoord board player color
        dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
        negamaxCall = \dstCoord -> -negamax (updateBoard board srcCoord dstCoord)
                                            player
                                            player
                                            (cellColorAt board dstCoord)
                                            depth
        valueDstCoordPairs = zip (map negamaxCall dstCoords) dstCoords
        bestDstCoord = if null valueDstCoordPairs then Nothing
                       else Just $ snd $ maximum $ valueDstCoordPairs

isTerminal :: Board -> Bool
isTerminal board =
  isWinning board KWhite || isWinning board KBlack

negamax :: Board -> Player -> Player -> KColor -> Int -> Double
-- initPlayer: invariant player who initiated the search (the board
--             value is always computed from his perspective)
-- currPlayer: who just played whose move resulted in the `board`
-- nextColor: of cell of last move made by `currPlayer`, thus
--            determining the piece of next player's move
negamax board initPlayer currPlayer nextColor depth
  | depth == 0 || isTerminal board =
      fn $ boardValueForPlayer board initPlayer nextColor
  | otherwise =
      (case null dstCoords of
        True ->
          -- board stays the same (src == dst), and next color is src's one
          negamax board initPlayer nextPlayer (cellColorAt board srcCoord) (depth - 1)
        False ->
          maximum $ map nextNegamaxCall dstCoords)
      where nextPlayer = if currPlayer == KWhite then KBlack else KWhite
            srcCoord = getPlayerPieceColorCoord board nextPlayer nextColor
            dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
            nextNegamaxCall = \dstCoord -> -negamax (updateBoard board srcCoord dstCoord)
                                                    initPlayer
                                                    nextPlayer
                                                    (cellColorAt board dstCoord)
                                                    (depth - 1)
            fn = if currPlayer == initPlayer then negate else id

isLegalMove :: Board -> Player -> Maybe KColor -> Coord -> Maybe Coord -> Bool
isLegalMove _ _ _ _ Nothing = True
isLegalMove board player color src dst =
  playerOK && colorOK && srcOK && srcColorOK && dstOK
  where playerOK = (playerAt board src) == Just player
        colorOK = (color == Nothing) || (pieceColorAt board src) == color
        srcOK = (pieceAt board src) /= Nothing
        srcColorOK = (color == Nothing) ||
                     (getPlayerPieceColorCoord board player (fromJust color)) == src
        dstOK = fromJust dst `elem` getPossibleCoordsFromCoord board src

isBlocked :: Board -> Player -> KColor -> Bool
isBlocked board player color =
  --trace (show dstCoords)
  null dstCoords
  where srcCoord = getPlayerPieceColorCoord board player color
        dstCoords = getPossibleCoordsFromCoord board srcCoord
