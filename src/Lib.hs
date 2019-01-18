module Lib
    ( formatGrid
    , outputGrid
    , findWord
    , findWords
    , findWordInLine
    , findWordInCellLinePrefix
    , skew
    , zipOverGrid
    , zipOverGridWith
    , gridWithCoords
    , cell2char
    , Cell(Cell, Indent)
    , Game(gameGrid, gameWords)
    , makeGame
    , totalWords
    , score
    , completed
    , playGame
    , formatGame
    , makeRandomGrid
    , fillInBlanks
    ) where

import Data.List (isInfixOf,transpose)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Char (toLower)
import System.Random
import qualified Data.Map as M


type Grid a = [[ a ]]

data Cell = Cell (Integer, Integer) Char 
          | Indent
          deriving (Eq, Ord, Show)

data Game = Game {
              gameGrid :: Grid Cell,
              gameWords :: M.Map String (Maybe [Cell])
            } deriving Show

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game -- filter substrings of the words and words that are not in the dict
playGame game word =
  let grid = gameGrid game
      foundWord = findWord grid word
  in case foundWord of
    Nothing -> game
    Just cs -> 
      let dict = gameWords game
          newDict = M.insert word foundWord dict
      in game { gameWords = newDict } -- TODO: check how the {} operation works

formatGame :: Game -> String
formatGame game = formatGameGrid game
                  ++ "\n\n"
                  ++ (show $ score game)
                  ++ "/"
                  ++ (show $ totalWords game)

formatGameGrid :: Game -> String
formatGameGrid game = 
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell = 
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = mapOverGrid formatCell grid
  in unlines charGrid

fillInBlanks gen grid = 
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _ = c
  in zipOverGridWith fill grid r

makeRandomGrid gen = 
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
  in row : makeRandomGrid gen2

zipOverGrid :: Grid a -> Grid b -> Grid (a,b)
zipOverGrid = zipWith zip

zipOverGridWith :: (a -> b -> c) -> Grid a -> Grid b -> Grid c
zipOverGridWith = zipWith . zipWith

coordsGrid :: Grid (Integer, Integer)
coordsGrid = 
  let rows = map repeat [0..]
      cols = repeat [0..]
  in zipOverGrid rows cols

mapOverGrid :: (a -> b) -> Grid a -> Grid b
mapOverGrid = map . map

gridWithCoords :: Grid Char -> Grid Cell
gridWithCoords grid = zipOverGridWith Cell coordsGrid grid

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)

formatGrid :: Grid Cell -> String
formatGrid = unlines . mapOverGrid cell2char

cell2char :: Cell -> Char
cell2char (Cell _ c) = c
cell2char Indent = '?'

getLines :: Grid Cell -> [[Cell]]
getLines grid = 
  let horizonal = grid
      vertical = transpose grid
      diagonalRight = diagonalise grid 
      diagonalLeft = diagonalise (map reverse grid)
      lines = horizonal ++ vertical ++ diagonalRight ++ diagonalLeft
  in lines ++ (map reverse lines) 

diagonalise :: Grid Cell -> Grid Cell
diagonalise = ( transpose . skew )

skew :: Grid Cell -> Grid Cell
skew [] = []
skew ( l:ls ) = l : skew ( map indent ls )
  where indent line = Indent : line

findWord :: Grid Cell -> String -> Maybe [Cell]
findWord grid word = 
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWords :: Grid Cell -> [String] -> [[Cell]]
findWords grid words = catMaybes $ map (findWord grid) words

findWordInLine :: String -> [ Cell ] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line = 
  let found = findWordInCellLinePrefix [] word line
  in case found of 
    Nothing -> findWordInLine word (tail line)
    Just cs -> Just cs

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c = findWordInCellLinePrefix (c:acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing