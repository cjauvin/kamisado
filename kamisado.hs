{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import qualified Data.Sequence as Sequence
import Data.Maybe
import Data.List
import Data.Foldable (toList)
import Debug.Trace
import Control.Exception (assert)
import Control.Monad
import System.Console.ANSI as ANSI

fromList = Sequence.fromList
index = Sequence.index
update = Sequence.update

-- http://stackoverflow.com/questions/31106484/pattern-matching-data-sequence-like-lists
pattern Empty   <- (Sequence.viewl -> Sequence.EmptyL)  where Empty = Sequence.empty
pattern x :< xs <- (Sequence.viewl -> x Sequence.:< xs) where (:<)  = (Sequence.<|)
pattern xs :> x <- (Sequence.viewr -> xs Sequence.:> x) where (:>)  = (Sequence.|>)

data KColor = KRed |
              KGreen |
              KBlue |
              KYellow
             deriving (Eq, Show)

toColor :: KColor -> Color
toColor KRed = Red
toColor KGreen = Green
toColor KBlue = Blue
toColor KYellow = Yellow

data Player = KWhite |
              KBlack
             deriving (Eq, Show)

getPlayerStr :: Maybe Player -> String
getPlayerStr (Just KWhite) = "X"
getPlayerStr (Just KBlack) = "O"
getPlayerStr Nothing = " "

data Piece = Piece {
    getPlayer :: Player,
    getColor :: KColor
  } deriving (Eq, Show)

type Seq = Sequence.Seq
type Cell = (KColor, Maybe Piece)
type Board = Seq (Seq (Cell))
type Coord = (Int, Int)

inf = (read "Infinity")::Double

initBoard :: Board
initBoard = fromList [
  fromList [(KRed, Just $ Piece KBlack KRed), (KGreen, Just $ Piece KBlack KGreen),
            (KBlue, Just $ Piece KBlack KBlue), (KYellow, Just $ Piece KBlack KYellow)],
  fromList [(KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing), (KBlue, Nothing)],
  fromList [(KBlue, Nothing), (KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing)],
  fromList [(KGreen, Just $ Piece KWhite KGreen), (KBlue, Just $ Piece KWhite KBlue),
            (KYellow, Just $ Piece KWhite KYellow), (KRed, Just $ Piece KWhite KRed)]]

gridSize = length initBoard

cellAt :: Board -> Coord -> Cell
cellAt board (i, j) = index (index board i) j
cellColorAt :: Board -> Coord -> KColor
cellColorAt board (i, j) = fst (cellAt board (i, j))
pieceAt :: Board -> Coord -> Maybe Piece
pieceAt board (i, j) = snd (cellAt board (i, j))
pieceColorAt :: Board -> Coord -> Maybe KColor
pieceColorAt board (i, j) = fmap getColor $ pieceAt board (i, j)
playerAt :: Board -> Coord -> Maybe Player
playerAt board (i, j) = fmap getPlayer $ pieceAt board (i, j)
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
  where playersInWinningRow = fmap (fmap getPlayer) playerWinningRow
        playerWinningRow = fmap snd $ index board rowIdx
        rowIdx = case player of
          KWhite -> 0
          KBlack -> gridSize - 1

hasOpenPath :: Board -> Coord -> Bool
hasOpenPath board (i, j) =
  not $ null $ filter (\(i, _) -> i == playerWinningRowIdx) moves
  where player = fromJust $ playerAt board (i, j)
        moves = getPossibleCoordsFromCoord board (i, j)
        playerWinningRowIdx = case player of
          KWhite -> 0
          KBlack -> gridSize - 1

getBoardPotentialValueForPlayer :: Board -> Player -> Double
getBoardPotentialValueForPlayer board player =
  fromIntegral $ length $ filter (\(i, j) -> hasOpenPath board (i, j)) coords
  where coords = getPlayerPieceCoords board player

boardValueForPlayer :: Board -> Player -> Double
boardValueForPlayer board player =
  winning - losing + potentialPlayer - potentialOpponent
  where winning = if isWinning board player
                  then inf else 0
        losing =  if isWinning board opponent
                  then inf else 0
        potentialPlayer = getBoardPotentialValueForPlayer board player
        potentialOpponent = getBoardPotentialValueForPlayer board opponent
        opponent = if player == KWhite then KBlack else KWhite

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
-- initPlayer: invariant player who initiated the search (the board value is always computed from his perspective)
-- currPlayer: who just played whose move resulted in the `board`
-- nextColor:  of cell of last move made by `currPlayer`, thus determining the piece of next player's move
negamax board initPlayer currPlayer nextColor depth
  | depth == 0 || isTerminal board =
      fn $ boardValueForPlayer board initPlayer
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

printBoardRowCells :: Seq (Cell) -> IO ()
printBoardRowCells (c:<cs) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (toColor $ fst c)]
  putStr "     "
  printBoardRowCells cs
printBoardRowCells Empty = do
  setSGR [Reset]
  putStrLn ""

printBoardRowPieces :: Seq (Cell) -> IO ()
printBoardRowPieces (c:<cs) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (toColor $ fst c)]
  putStr "  "
  let piece = snd c
  case piece of
    Nothing -> do
      setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (toColor $ fst c)]
      putStr " "
    _       -> do
      setSGR [Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid (toColor $ getColor $ fromJust piece)]
      putStr (getPlayerStr (fmap getPlayer piece))
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (toColor $ fst c)]
  putStr "  "
  printBoardRowPieces cs
printBoardRowPieces Empty = do
  setSGR [Reset]
  putStrLn ""

printBoardRows :: Board -> Int -> IO ()
printBoardRows Empty _ = return ()
printBoardRows (r:<rs) i =
  do putStr "  "
     printBoardRowCells r
     putStr $ " " ++ (show i)
     printBoardRowPieces r
     putStr "  "
     printBoardRowCells r
     printBoardRows rs (i + 1)

printBoardTopCoords :: Int -> Int -> IO ()
printBoardTopCoords i n
  | i == n    = putStrLn ""
  | otherwise =
    do
      putStr ("  " ++ (show i) ++ "  ")
      printBoardTopCoords (i + 1) n

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn ""
  putStr "  "
  printBoardTopCoords 0 $ length board
  printBoardRows board 0
  putStrLn ""

cpuPlay :: Board -> KColor -> Int -> IO ()
cpuPlay board color depth = do
  let cpuSrcCoord = getPlayerPieceColorCoord board KBlack color
      cpuDstCoord = findBestMoveCoord board KBlack color depth
  case cpuDstCoord == Nothing of
    True -> do
      printBoard board
      putStrLn ("CPU blocked, your turn again" ++ " [" ++ (show color) ++ "]")
      play board (Just color) depth
    False -> do
      let cpuBoard = updateBoard board cpuSrcCoord $ fromJust cpuDstCoord
          cpuColor = cellColorAt cpuBoard $ fromJust cpuDstCoord
          isCpuWinning = isWinning cpuBoard KBlack
      putStrLn ("CPU: " ++ (show cpuSrcCoord) ++ " -> " ++ (show $ fromJust cpuDstCoord) ++ " [" ++ (show cpuColor) ++ "]")
      printBoard cpuBoard
      case isCpuWinning of
        True -> do
          putStrLn "CPU won!"
          return ()
        False -> do
          case isBlocked cpuBoard KWhite cpuColor of
            True -> do
              let humanVoidMoveCoord = getPlayerPieceColorCoord cpuBoard KWhite cpuColor
              let humanVoidMoveColor = cellColorAt cpuBoard humanVoidMoveCoord
              putStr ("You are blocked, the CPU will play again.." ++ " [" ++ (show humanVoidMoveColor) ++ "]")
              getLine
              cpuPlay cpuBoard humanVoidMoveColor depth
            False -> do
              play cpuBoard (Just cpuColor) depth

play :: Board -> Maybe KColor -> Int -> IO ()
play board color depth = do
  line <- getLine
  unless (line == "q") $ do
    let (humanSrcCoord, humanDstCoord) = (read line::(Coord, Coord))
        isHumanMoveLegal = isLegalMove board KWhite color humanSrcCoord $ Just humanDstCoord
    case isHumanMoveLegal of
      False -> do
        putStrLn "illegal move"
        play board color depth
      True -> do
        let humanBoard = updateBoard board humanSrcCoord humanDstCoord
            humanColor = cellColorAt humanBoard humanDstCoord
            isHumanWinning = isWinning humanBoard KWhite
        case isHumanWinning of
          True -> do
              printBoard humanBoard
              putStrLn "You won!"
              return ()
          False -> do
            cpuPlay humanBoard humanColor depth

main :: IO ()
main = do
  printBoard initBoard
  play initBoard Nothing 3
