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

getColor :: KColor -> Color
getColor KRed = Red
getColor KGreen = Green
getColor KBlue = Blue
getColor KYellow = Yellow

data KPlayer = KWhite |
              KBlack
             deriving (Eq, Show)

getKPlayerStr :: Maybe KPlayer -> String
getKPlayerStr (Just KWhite) = "X"
getKPlayerStr (Just KBlack) = "O"
getKPlayerStr Nothing = " "

data KPiece = KPiece {
    getKPlayer :: KPlayer,
    getKColor :: KColor
  } deriving (Eq, Show)

type Seq = Sequence.Seq
type Cell = (KColor, Maybe KPiece)
type Board = Seq (Seq (Cell))
type Coord = (Int, Int)

inf = (read "Infinity")::Double

initBoard :: Board
-- initBoard = fromList [
--   fromList [(KRed, Just $ KPiece KBlack KRed), (KGreen, Just $ KPiece KBlack KGreen),
--             (KBlue, Just $ KPiece KBlack KBlue), (KYellow, Just $ KPiece KBlack KYellow)],
--   fromList [(KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing), (KBlue, Nothing)],
--   fromList [(KBlue, Nothing), (KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing)],
--   fromList [(KGreen, Just $ KPiece KWhite KGreen), (KBlue, Just $ KPiece KWhite KBlue),
--             (KYellow, Just $ KPiece KWhite KYellow), (KRed, Just $ KPiece KWhite KRed)]]

-- CPU blocked
-- weird: ((3,0),(2,0)), [cpu blocked], ((3,1),(2,1))
initBoard = fromList [
  fromList [(KRed, Just $ KPiece KBlack KRed), (KGreen, Just $ KPiece KBlack KGreen),
            (KBlue, Nothing), (KYellow, Just $ KPiece KBlack KYellow)],
  fromList [(KYellow, Nothing), (KRed, Nothing), (KGreen, Nothing), (KBlue, Nothing)],
  fromList [(KBlue, Nothing), (KYellow, Nothing), (KRed, Just $ KPiece KBlack KBlue), (KGreen, Nothing)],
  fromList [(KGreen, Just $ KPiece KWhite KGreen), (KBlue, Just $ KPiece KWhite KBlue),
            (KYellow, Just $ KPiece KWhite KYellow), (KRed, Just $ KPiece KWhite KRed)]]

-- human blocked
-- initBoard = fromList [
--   fromList [(KRed, Just $ KPiece KBlack KRed), (KGreen, Just $ KPiece KBlack KGreen),
--             (KBlue, Just $ KPiece KBlack KBlue), (KYellow, Just $ KPiece KBlack KYellow)],
--   fromList [(KYellow, Nothing), (KRed, Just $ KPiece KWhite KRed), (KGreen, Just $ KPiece KWhite KGreen),
--             (KBlue, Nothing)],
--   fromList [(KBlue, Just $ KPiece KWhite KYellow), (KYellow, Nothing), (KRed, Nothing), (KGreen, Just $ KPiece KWhite KBlue)],
--   fromList [(KGreen, Nothing), (KBlue, Nothing), (KYellow, Nothing), (KRed, Nothing)]]

gridSize = length initBoard

b0 = initBoard
b1 = updateBoard initBoard (3, 0) (2, 0) -- KWhite played
b2 = updateBoard b1 (3, 1) (2, 1)
--b3 = updateBoard b2 (2, 0) (1, 0)
--b4 = updateBoard b4 (0, 3) (2, 3)

cellAt :: Board -> Coord -> Cell
cellAt board (i, j) = index (index board i) j
cellKColorAt :: Board -> Coord -> KColor
cellKColorAt board (i, j) = fst (cellAt board (i, j))
pieceAt :: Board -> Coord -> Maybe KPiece
pieceAt board (i, j) = snd (cellAt board (i, j))
pieceKColorAt :: Board -> Coord -> Maybe KColor
pieceKColorAt board (i, j) = fmap getKColor $ pieceAt board (i, j)
playerAt :: Board -> Coord -> Maybe KPlayer
playerAt board (i, j) = fmap getKPlayer $ pieceAt board (i, j)
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
  where srcCell = (cellKColorAt board (i0, j0), Nothing)
        srcRow = update j0 srcCell $ index board i0
        dstCell = (cellKColorAt board (i1, j1), pieceAt board (i0, j0))
        dstRow = update j1 dstCell $ index board i1
        dstBoard = update i1 dstRow $ board

getKPlayerKPieceCoords :: Board -> KPlayer -> [Coord]
getKPlayerKPieceCoords board player =
  filter (\(i, j) -> (playerAt board (i, j)) == Just player) coords
  where coords = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

getKPlayerKPieceKColorCoord :: Board -> KPlayer -> KColor -> Coord
getKPlayerKPieceKColorCoord board player color =
  fromJust $ find (\(i, j) -> (pieceAt board (i, j)) == (Just $ KPiece player color)) coords
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

isWinning :: Board -> KPlayer -> Bool
isWinning board player =
  any (Just player ==) playersInWinningRow
  where playersInWinningRow = fmap (fmap getKPlayer) playerWinningRow
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

getBoardPotentialValueForKPlayer :: Board -> KPlayer -> Double
getBoardPotentialValueForKPlayer board player =
  fromIntegral $ length $ filter (\(i, j) -> hasOpenPath board (i, j)) coords
  where coords = getKPlayerKPieceCoords board player

boardValueForKPlayer :: Board -> KPlayer -> Double
boardValueForKPlayer board player =
  winning - losing + potentialPlayer - potentialOpponent
  where winning = if isWinning board player
                  then inf else 0
        losing =  if isWinning board opponent
                  then inf else 0
        potentialPlayer = getBoardPotentialValueForKPlayer board player
        potentialOpponent = getBoardPotentialValueForKPlayer board opponent
        opponent = if player == KWhite then KBlack else KWhite

findBestMoveCoord :: Board -> KPlayer -> KColor -> Int -> Maybe Coord
findBestMoveCoord board player color depth =
  assert (isLegalMove board player (Just color) srcCoord bestDstCoord)
  --trace (show srcCoord)
  -- trace "==="
  trace (show valueDstCoordPairs)
  bestDstCoord
  where srcCoord = getKPlayerKPieceKColorCoord board player color
        --dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
        --dstCoords = [(1, 0)]
        --dstCoords = [(0,2)] -- White=INF
        dstCoords = [(1,1)] -- Black=INF, White=-INF
        negamaxCall = \dstCoord -> -negamax (updateBoard board srcCoord dstCoord)
                                           player
                                           (cellKColorAt board dstCoord)
                                           depth
                                           True
                                           (srcCoord, dstCoord)
        valueDstCoordPairs = zip (map negamaxCall dstCoords) dstCoords
        bestDstCoord = if null valueDstCoordPairs then Nothing
                       else Just $ snd $ maximum $ valueDstCoordPairs

negamax :: Board -> KPlayer -> KColor -> Int -> Bool -> (Coord, Coord) -> Double
-- player: `player` who just played whose move resulted in the `board`
-- color:  `color` of cell of last move made by `player`, thus determining the piece of next player's move
negamax board player color depth isNegate (src, dst)
  | depth == 0 || (isWinning board player) =
      trace (show (depth, (isWinning board player), player, color, isNegate, (boardValueForKPlayer board player), src, dst))
      boardValueForKPlayer board player
  | otherwise =
      --trace (show (player, color, depth, srcCoord, dstCoords))
      (case null dstCoords of
        True ->
          trace (show (depth, "+++", player, color))
          negamax board nextKPlayer (cellKColorAt board srcCoord) (depth - 1) (not isNegate) (srcCoord, srcCoord)
        False ->
          trace (show (depth, "---", player, color, res2))
          maximum res2)
      where nextKPlayer = if player == KWhite then KBlack else KWhite
            srcCoord = getKPlayerKPieceKColorCoord board nextKPlayer color
            dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
            fn = if isNegate then negate else id
            nextNegamaxCall = \dstCoord -> -negamax (updateBoard board srcCoord dstCoord)
                                                    nextKPlayer
                                                    (cellKColorAt board dstCoord)
                                                    (depth - 1)
                                                    (not isNegate)
                                                    (srcCoord, dstCoord)
            res = map nextNegamaxCall dstCoords
            res2 = map fn res

findBestMoveCoord2 :: Board -> KPlayer -> KColor -> Int -> Maybe Coord
findBestMoveCoord2 board player color depth =
  assert (isLegalMove board player (Just color) srcCoord bestDstCoord)
  --trace (show srcCoord)
  -- trace "==="
  trace (show valueDstCoordPairs)
  bestDstCoord
  where srcCoord = getKPlayerKPieceKColorCoord board player color
        dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
        --dstCoords = [(1, 0)]
        --dstCoords = [(0,2)] -- White=INF
        --dstCoords = [(1,1)] -- White=INF
        --dstCoords = [(1,1), (0,2)] -- Black=INF, White=-INF
        negamaxCall = \dstCoord -> -negamax2 (updateBoard board srcCoord dstCoord)
                                             player
                                             player
                                             (cellKColorAt board dstCoord)
                                             depth
        valueDstCoordPairs = zip (map negamaxCall dstCoords) dstCoords
        bestDstCoord = if null valueDstCoordPairs then Nothing
                       else Just $ snd $ maximum $ valueDstCoordPairs

isTerminal :: Board -> Bool
isTerminal board =
  isWinning board KWhite || isWinning board KBlack

negamax2 :: Board -> KPlayer -> KPlayer -> KColor -> Int -> Double
-- initPlayer: invariant player who initiated the search (the board value is always computed from his perspective)
-- currPlayer: who just played whose move resulted in the `board`
-- nextColor:  of cell of last move made by `currPlayer`, thus determining the piece of next player's move
negamax2 board initPlayer currPlayer nextColor depth
  | depth == 0 || isTerminal board =
      --trace (show board)
      --trace (show (depth, (isTerminal board), initPlayer, currPlayer, color, (boardValueForKPlayer board initPlayer)))
      fn $ boardValueForKPlayer board initPlayer
  | otherwise =
      (case null dstCoords of
        True ->
          -- board stays the same (src == dst), and next color is src's one
          negamax2 board initPlayer nextPlayer (cellKColorAt board srcCoord) (depth - 1)
        False ->
          --trace (show (depth, "---", player, color, res2))
          maximum $ map nextNegamaxCall dstCoords)
      where nextPlayer = if currPlayer == KWhite then KBlack else KWhite
            srcCoord = getKPlayerKPieceKColorCoord board nextPlayer nextColor
            dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
            nextNegamaxCall = \dstCoord -> -negamax2 (updateBoard board srcCoord dstCoord)
                                                    initPlayer
                                                    nextPlayer
                                                    (cellKColorAt board dstCoord)
                                                    (depth - 1)
            fn = if currPlayer == initPlayer then negate else id

isLegalMove :: Board -> KPlayer -> Maybe KColor -> Coord -> Maybe Coord -> Bool
isLegalMove _ _ _ _ Nothing = True
isLegalMove board player color src dst =
  playerOK && colorOK && srcOK && srcKColorOK && dstOK
  where playerOK = (playerAt board src) == Just player
        colorOK = (color == Nothing) || (pieceKColorAt board src) == color
        srcOK = (pieceAt board src) /= Nothing
        srcKColorOK = (color == Nothing) ||
                     (getKPlayerKPieceKColorCoord board player (fromJust color)) == src
        dstOK = fromJust dst `elem` getPossibleCoordsFromCoord board src

isBlocked :: Board -> KPlayer -> KColor -> Bool
isBlocked board player color =
  --trace (show dstCoords)
  null dstCoords
  where srcCoord = getKPlayerKPieceKColorCoord board player color
        dstCoords = getPossibleCoordsFromCoord board srcCoord

printBoardRowCells :: Seq (Cell) -> IO ()
printBoardRowCells (c:<cs) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (getColor $ fst c)]
  putStr "     "
  printBoardRowCells cs
printBoardRowCells Empty = do
  setSGR [Reset]
  putStrLn ""

printBoardRowPieces :: Seq (Cell) -> IO ()
printBoardRowPieces (c:<cs) = do
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (getColor $ fst c)]
  putStr "  "
  let piece = snd c
  case piece of
    Nothing -> do
      setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (getColor $ fst c)]
      putStr " "
    _       -> do
      setSGR [Reset, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid (getColor $ getKColor $ fromJust piece)]
      putStr (getKPlayerStr (fmap getKPlayer piece))
  setSGR [SetConsoleIntensity BoldIntensity, SetColor Background Vivid (getColor $ fst c)]
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
  let cpuSrcCoord = getKPlayerKPieceKColorCoord board KBlack color
      cpuDstCoord = findBestMoveCoord board KBlack color depth
  case cpuDstCoord == Nothing of
    True -> do
      printBoard board
      putStrLn ("CPU blocked, your turn again" ++ " [" ++ (show color) ++ "]")
      play board (Just color) depth
    False -> do
      let cpuBoard = updateBoard board cpuSrcCoord $ fromJust cpuDstCoord
          cpuKColor = cellKColorAt cpuBoard $ fromJust cpuDstCoord
          isCpuWinning = isWinning cpuBoard KBlack
      putStrLn ("CPU: " ++ (show cpuSrcCoord) ++ " -> " ++ (show $ fromJust cpuDstCoord) ++ " [" ++ (show cpuKColor) ++ "]")
      printBoard cpuBoard
      case isCpuWinning of
        True -> do
          putStrLn "CPU won!"
          return ()
        False -> do
          case isBlocked cpuBoard KWhite cpuKColor of
            True -> do
              putStrLn "You are blocked, the CPU will play again"
              cpuPlay cpuBoard cpuKColor depth
            False -> do
              play cpuBoard (Just cpuKColor) depth

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
            humanKColor = cellKColorAt humanBoard humanDstCoord
            isHumanWinning = isWinning humanBoard KWhite
        case isHumanWinning of
          True -> do
              printBoard humanBoard
              putStrLn "You won!"
              return ()
          False -> do
            cpuPlay humanBoard humanKColor depth

main :: IO ()
main = do
  printBoard b2
  play b2 Nothing 3
