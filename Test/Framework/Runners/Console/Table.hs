module Test.Framework.Runners.Console.Table (
        Cell(..), Column(..), renderTable
    ) where

import Test.Framework.Utilities


data Cell = Text String
          | Seperator

data Column = Column [Cell]
            | SeperatorColumn

type ColumnWidth = Int

renderTable :: [Column] -> String
renderTable = renderColumnsWithWidth . map (\column -> (findColumnWidth column, column))


findColumnWidth :: Column -> Int
findColumnWidth SeperatorColumn = 0
findColumnWidth (Column cells)  = maximum (map findCellWidth cells)

findCellWidth :: Cell -> Int
findCellWidth (Text s)  = length s
findCellWidth Seperator = 0


renderColumnsWithWidth :: [(ColumnWidth, Column)] -> String
renderColumnsWithWidth columns
  | all (columnFinished . snd) columns
  = ""
  | otherwise
  = first_cells_str ++ "\n" ++
    renderColumnsWithWidth (map (onRight columnDropHead) columns)
  where
    first_cells_str = concat $ zipWith (uncurry renderFirstColumnCell) columns (eitherSideSeperator (map snd columns))


eitherSideSeperator :: [Column] -> [Bool]
eitherSideSeperator columns = zipWith (||) (False:column_is_seperator) (tail column_is_seperator ++ [False])
  where
    column_is_seperator = map isSeperatorColumn columns

isSeperatorColumn :: Column -> Bool
isSeperatorColumn SeperatorColumn = False
isSeperatorColumn (Column cells)  = case cells of
    []       -> False
    (cell:_) -> isSeperatorCell cell

isSeperatorCell :: Cell -> Bool
isSeperatorCell Seperator = True
isSeperatorCell _         = False


renderFirstColumnCell :: ColumnWidth -> Column -> Bool -> String
renderFirstColumnCell column_width (Column cells) _ = case cells of
    []                -> replicate (column_width + 2) ' '
    (Seperator:_)     -> replicate (column_width + 2) '-'
    (Text contents:_) -> " " ++ padRight column_width contents ++ " "
renderFirstColumnCell _ SeperatorColumn either_side_seperator 
  = if either_side_seperator then "+" else "|"

columnFinished :: Column -> Bool
columnFinished (Column cells)  = null cells
columnFinished SeperatorColumn = True

columnDropHead :: Column -> Column
columnDropHead (Column cells)  = Column (drop 1 cells)
columnDropHead SeperatorColumn = SeperatorColumn