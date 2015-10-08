{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Char
import Data.Foldable (toList)
import Debug.Trace
import Control.Exception (assert)
import Control.Monad
import System.Console.ANSI as ANSI
import System.IO

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
             deriving (Eq, Ord, Show)

toColor :: KColor -> (Color, ColorIntensity)
toColor KOrange = (Red, Vivid)
toColor KBlue = (Cyan, Dull)
toColor KPurple = (Blue, Dull)
toColor KPink = (Magenta, Vivid)
toColor KYellow = (Yellow, Vivid)
toColor KRed = (Red, Dull)
toColor KGreen = (Green, Dull)
toColor KBrown = (Black, Vivid)

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

printBoardRowCells :: Seq (Cell) -> IO ()
printBoardRowCells (c:<cs) = do
  let (cellColor, cellIntensity) = toColor $ fst c
  setSGR [SetConsoleIntensity BoldIntensity,
          SetColor Background cellIntensity cellColor]
  putStr "     "
  printBoardRowCells cs
printBoardRowCells Empty = do
  setSGR [Reset]
  putStrLn ""

printBoardRowPieces :: Seq (Cell) -> IO ()
printBoardRowPieces (c:<cs) = do
  let (cellColor, cellIntensity) = toColor $ fst c
      piece = snd c
      (pieceColor, pieceIntensity) = toColor $ getColor $ fromJust piece
  setSGR [SetConsoleIntensity BoldIntensity,
          SetColor Background cellIntensity cellColor]
  putStr "  "
  case piece of
    Nothing -> do
      setSGR [SetConsoleIntensity BoldIntensity,
              SetColor Background cellIntensity cellColor]
      putStr " "
    _       -> do
      setSGR [Reset, SetConsoleIntensity BoldIntensity,
              SetColor Foreground pieceIntensity pieceColor]
      putStr (getPlayerStr (getPlayer <$> piece))
  setSGR [SetConsoleIntensity BoldIntensity,
          SetColor Background cellIntensity cellColor]
  putStr "  "
  printBoardRowPieces cs
printBoardRowPieces Empty = do
  setSGR [Reset]
  --putStrLn ""

printBoardRows :: Board -> Int -> IO ()
printBoardRows Empty _ = return ()
printBoardRows (r:<rs) i =
  do putStr "  "
     printBoardRowCells r
     putStr $ " " ++ (show i)
     printBoardRowPieces r
     putStrLn $ show i
     putStr "  "
     printBoardRowCells r
     printBoardRows rs (i + 1)

printBoardColumnCoords :: Int -> Int -> IO ()
printBoardColumnCoords i n
  | i == n    = putStrLn ""
  | otherwise =
    do
      putStr ("  " ++ (show i) ++ "  ")
      printBoardColumnCoords (i + 1) n

printBoard :: Board -> IO ()
printBoard board = do
  putStrLn ""
  putStr "  "
  printBoardColumnCoords 0 $ length board
  printBoardRows board 0
  putStr "  "
  printBoardColumnCoords 0 $ length board
  putStrLn ""

cpuPlay :: Board -> KColor -> Int -> IO ()
cpuPlay board color depth = do
  putStr "CPU is thinking.."
  let cpuSrcCoord = getPlayerPieceColorCoord board KBlack color
      cpuDstCoord = findBestMoveCoord board KBlack color depth
  putStrLn ""
  case cpuDstCoord == Nothing of
    True -> do
      -- CPU "zero" move, where src==dst
      let cpuVoidMoveColor = cellColorAt board cpuSrcCoord
      printBoard board
      putStrLn ("CPU blocked, your turn again" ++ " [" ++ (show cpuVoidMoveColor) ++ "]")
      play board cpuVoidMoveColor depth
    False -> do
      let cpuBoard = updateBoard board cpuSrcCoord $ fromJust cpuDstCoord
          cpuColor = cellColorAt cpuBoard $ fromJust cpuDstCoord
          isCpuWinning = isWinning cpuBoard KBlack
      putStrLn ("CPU: " ++ (show cpuSrcCoord) ++ " -> " ++
                (show $ fromJust cpuDstCoord) ++ " [" ++ (show cpuColor) ++ "]")
      printBoard cpuBoard
      case isCpuWinning of
        True -> do
          putStrLn "CPU won!"
          return ()
        False -> do
          case isBlocked cpuBoard KWhite cpuColor of
            True -> do
              -- Human "zero" move, where src==dst
              let humanVoidMoveCoord = getPlayerPieceColorCoord cpuBoard KWhite cpuColor
              let humanVoidMoveColor = cellColorAt cpuBoard humanVoidMoveCoord
              putStr ("You are blocked, the CPU will play again.." ++
                      " [" ++ (show humanVoidMoveColor) ++ "]")
              getLine
              cpuPlay cpuBoard humanVoidMoveColor depth
            False -> do
              play cpuBoard cpuColor depth

parseHumanSrcAndDstCoords :: String -> (Coord, Coord)
parseHumanSrcAndDstCoords str =
  ((digits !! 0, digits !! 1), (digits !! 2, digits !! 3))
  where digits = map (read::String -> Int) $ wordsBy (not . isDigit) str

parseHumanDstCoords :: String -> Coord
parseHumanDstCoords str =
  (digits !! 0, digits !! 1)
  where digits = map (read::String -> Int) $ wordsBy (not . isDigit) str

playFirst :: Board -> Int -> IO ()
playFirst board depth = do
  putStr "Enter your move (src/dst coords as 4 numbers): "
  hFlush stdout
  line <- getLine
  unless (line == "q") $ do
    let (humanSrcCoord, humanDstCoord) = parseHumanSrcAndDstCoords(line)
        isHumanMoveLegal = isLegalMove board KWhite Nothing
                                       humanSrcCoord $ Just humanDstCoord
    case isHumanMoveLegal of
      False -> do
        putStrLn "illegal move"
        playFirst board depth
      True -> do
        let humanBoard = updateBoard board humanSrcCoord humanDstCoord
            humanColor = cellColorAt humanBoard humanDstCoord
            isHumanWinning = isWinning humanBoard KWhite
        putStrLn ("You: " ++ (show humanSrcCoord) ++ " -> " ++
                  (show humanDstCoord) ++ " [" ++ (show humanColor) ++ "]")
        printBoard humanBoard
        case isHumanWinning of
          True -> do
              --printBoard humanBoard
              putStrLn "You won!"
              return ()
          False -> do
            cpuPlay humanBoard humanColor depth

play :: Board -> KColor -> Int -> IO ()
play board color depth = do
  putStr "Enter your move (dst coords as 2 numbers): "
  hFlush stdout
  line <- getLine
  let humanSrcCoord = getPlayerPieceColorCoord board KWhite color
  case line of
    -- quit
    "q" -> do
      return ()
    -- give the color and src coords of the piece that must be played
    "?" -> do
      putStrLn ("You have to play " ++ (show color) ++ ", at " ++ (show humanSrcCoord))
      play board color depth
    "!" -> do
      let bestHumanDstCoord = fromJust $ findBestMoveCoord board KWhite color depth
      putStrLn ("You best move would be " ++ (show bestHumanDstCoord) ++ ", from " ++ (show color))
      play board color depth
    -- print board (for debug)
    "b" -> do
      putStrLn $ show board
      play board color depth
    -- dst coords
    _  -> do
      let humanDstCoord = parseHumanDstCoords(line)
          --humanSrcCoord = getPlayerPieceColorCoord board KWhite color
          isHumanMoveLegal = isLegalMove board KWhite (Just color)
                                         humanSrcCoord $ Just humanDstCoord
      case isHumanMoveLegal of
        False -> do
          putStrLn "illegal move"
          play board color depth
        True -> do
          let humanBoard = updateBoard board humanSrcCoord humanDstCoord
              humanColor = cellColorAt humanBoard humanDstCoord
              isHumanWinning = isWinning humanBoard KWhite
          putStrLn ("You: " ++ (show humanSrcCoord) ++ " -> " ++
                    (show humanDstCoord) ++ " [" ++ (show humanColor) ++ "]")
          printBoard humanBoard
          case isHumanWinning of
            True -> do
                --printBoard humanBoard
                putStrLn "You won!"
                return ()
            False -> do
              cpuPlay humanBoard humanColor depth

main :: IO ()
main = do
  putStrLn "Welcome to Kamisado! You play White (X)"
  printBoard initBoard
  playFirst initBoard 3
