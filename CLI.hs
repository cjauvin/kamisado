import Control.Monad
import Data.Char
import Data.Maybe
import Data.List.Split
import Kamisado
import System.Console.ANSI as ANSI
import System.IO

toColor :: KColor -> (Color, ColorIntensity)
toColor KOrange = (Red, Vivid)
toColor KBlue = (Cyan, Dull)
toColor KPurple = (Blue, Dull)
toColor KPink = (Magenta, Vivid)
toColor KYellow = (Yellow, Vivid)
toColor KRed = (Red, Dull)
toColor KGreen = (Green, Dull)
toColor KBrown = (Black, Vivid)

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
