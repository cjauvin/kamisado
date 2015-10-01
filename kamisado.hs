import qualified Data.Sequence as Sequence
import Data.Maybe
import Data.List
import Debug.Trace
import Control.Exception (assert)
import Control.Monad

fromList = Sequence.fromList
index = Sequence.index
update = Sequence.update

data Color = Red |
             Green |
             Blue |
             Yellow
             deriving (Eq, Show)

data Player = White |
              Black
             deriving (Eq, Show)

data Piece = Piece {
    getPlayer :: Player,
    getColor :: Color
  } deriving (Eq, Show)

type Seq = Sequence.Seq
type Cell = (Color, Maybe Piece)
type Board = Seq (Seq (Cell))
type Coord = (Int, Int)

initBoard :: Board
initBoard = fromList [
  fromList [(Red, Just $ Piece Black Red), (Green, Just $ Piece Black Green),
            (Blue, Just $ Piece Black Blue), (Yellow, Just $ Piece Black Yellow)],
  fromList [(Yellow, Nothing), (Red, Nothing), (Green, Nothing), (Blue, Nothing)],
  fromList [(Blue, Nothing), (Yellow, Nothing), (Red, Nothing), (Green, Nothing)],
  fromList [(Green, Just $ Piece White Green), (Blue, Just $ Piece White Blue),
            (Yellow, Just $ Piece White Yellow), (Red, Just $ Piece White Red)]]

gridSize = length initBoard

b0 = initBoard
b1 = updateBoard initBoard (3, 0) (1, 0) -- White played, on Y
b2 = updateBoard b1 (0, 3) (3, 0) -- Black played, from Y

cellAt :: Board -> Coord -> Cell
cellAt board (i, j) = index (index board i) j
colorAt :: Board -> Coord -> Color
colorAt board (i, j) = fst (cellAt board (i, j))
pieceAt :: Board -> Coord -> Maybe Piece
pieceAt board (i, j) = snd (cellAt board (i, j))
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
  where srcCell = (colorAt board (i0, j0), Nothing)
        srcRow = update j0 srcCell $ index board i0
        dstCell = (colorAt board (i1, j1), pieceAt board (i0, j0))
        dstRow = update j1 dstCell $ index board i1
        dstBoard = update i1 dstRow $ board

getPlayerCoords :: Board -> Player -> [Coord]
getPlayerCoords board player =
  filter (\(i, j) -> (playerAt board (i, j)) == Just player) coords
  where coords = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

getPlayerColorCoord :: Board -> Player -> Color -> Coord
getPlayerColorCoord board player color =
  fromJust $ find (\(i, j) -> (pieceAt board (i, j)) == (Just $ Piece player color)) coords
  where coords = [(i, j) | i <- [0..gridSize-1], j <- [0..gridSize-1]]

getPossibleCoordsFromCoord :: Board -> Coord -> [Coord]
getPossibleCoordsFromCoord board (i, j) =
  case player of
    White -> concat $ map constrainMoves
             [upleftward (i, j), upward (i, j), uprightward (i, j)]
    Black -> concat $ map constrainMoves
             [downleftward (i, j), downward (i, j), downrightward (i, j)]
  where constrainMoves = takeWhile $ cellEmpty board
        player = fromJust $ playerAt board (i, j)

-- getBoardsForPotentialPlayerMoves :: Board -> Coord -> [Board]
-- getBoardsForPotentialPlayerMoves board (i0, j0) =
--   map (\(i1, j1) -> movePiece board ((i0, j0), (i1, j1))) moves
--   where player = fromJust $ playerAt board (i0, j0)
--         moves = getPossibleMovesForPiece board (i0, j0)

inf = (read "Infinity")::Double

isWinning :: Board -> Player -> Bool
isWinning board player =
  any (Just player ==) playersInWinningRow
  where playersInWinningRow = fmap (fmap getPlayer) (fmap snd $ index board playerWinningRow)
        playerWinningRow = case player of
          White -> 0
          Black -> gridSize - 1

hasOpenPath :: Board -> Coord -> Bool
hasOpenPath board (i, j) =
  not $ null $ filter (\(i, _) -> i == playerWinningRow) moves
  where player = fromJust $ playerAt board (i, j)
        moves = getPossibleCoordsFromCoord board (i, j)
        playerWinningRow = case player of
          White -> 0
          Black -> gridSize - 1

getBoardPotentialValueForPlayer :: Board -> Player -> Double
getBoardPotentialValueForPlayer board player =
  fromIntegral $ length $ filter (\(i, j) -> hasOpenPath board (i, j)) coords
  where coords = getPlayerCoords board player

boardValueForPlayer :: Board -> Player -> Double
boardValueForPlayer board player =
  winning + potential
  where winning = if isWinning board player
                  then inf else 0
        potential = getBoardPotentialValueForPlayer board player

findBestMoveCoord :: Board -> Player -> Color -> Int -> Coord
findBestMoveCoord board player color depth =
  snd $ maximum $ valueCoordPairs
  where srcCoord = getPlayerColorCoord board player color
        dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
        nextPlayer = if player == White then Black else White
        negamaxCall = \dstCoord -> negamax (updateBoard board srcCoord dstCoord)
                                           nextPlayer
                                           (colorAt board dstCoord)
                                           depth
        valueCoordPairs = zip (map negamaxCall dstCoords) dstCoords

negamax :: Board -> Player -> Color -> Int -> Double
negamax board player color depth
  | depth == 0 || (isWinning board player) = fn $ boardValueForPlayer board player
  | otherwise =
      --trace (show (player, color, depth, srcCoord, dstCoords))
      (case null dstCoords of
        True -> negamax board nextPlayer color (depth - 1)
        False -> maximum $ map nextNegamaxCall dstCoords)
      where srcCoord = getPlayerColorCoord board player color
            dstCoords = [(i, j) | (i, j) <- getPossibleCoordsFromCoord board srcCoord]
            nextPlayer = if player == White then Black else White
            fn = if player == White then negate else id
            nextNegamaxCall = \dstCoord -> -negamax (updateBoard board srcCoord dstCoord)
                                                    nextPlayer
                                                    (colorAt board dstCoord)
                                                    (depth - 1)

isLegalMove :: Board -> Player -> Maybe Color -> Coord -> Coord -> Bool
isLegalMove board player color src dst =
  colorOK && srcOK && playerOK && srcColorOK && dstOK
  where colorOK = (color == Nothing) || (Just $ colorAt board src) == color
        srcOK = (pieceAt board src) /= Nothing
        playerOK = (playerAt board src) == Just player
        srcColorOK = (color == Nothing) || (getPlayerColorCoord board player (fromJust color)) == src
        dstOK = dst `elem` getPossibleCoordsFromCoord board src

play :: Board -> Maybe Color -> IO ()
play board color = do
  line <- getLine
  unless (line == "q") $ do
    let (humanSrcCoord, humanDstCoord) = (read line::(Coord, Coord))
    if (not $ isLegalMove board White color humanSrcCoord humanDstCoord) then do
      putStrLn "illegal move"
      play board color
    else do
      let humanBoard = updateBoard board humanSrcCoord humanDstCoord
          humanColor = colorAt humanBoard humanDstCoord
          cpuSrcCoord = getPlayerColorCoord humanBoard Black humanColor
          cpuDstCoord = findBestMoveCoord humanBoard Black humanColor 2
          cpuBoard = updateBoard humanBoard cpuSrcCoord cpuDstCoord
          cpuColor = colorAt cpuBoard cpuDstCoord
      if (isWinning cpuBoard Black) then do
        putStrLn ("CPU: " ++ (show (cpuSrcCoord, cpuDstCoord)))
        putStrLn "CPU won"
        return ()
      else do
        putStrLn ("CPU: " ++ (show (cpuSrcCoord, cpuDstCoord)))
        play cpuBoard (Just cpuColor)

main :: IO ()
main = play initBoard Nothing
