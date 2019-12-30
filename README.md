# JigsawSudokuGameGUI

## Run in windows

1. under project root folder, run "cabal install .." necessary packages
2. "stack ghci"
3. "main" 

## Run in Mac
1. under project root folder, run "cabal install .." necessary packages
2. stack install
3. stack exec JigsawSudokuGameGUI-exe
4. full size game screen
4. shrink size game screen

## Save and Load board/game progress

1. only enter file name exclude suffix ".txt"
2. enter file name exclude suffix "-play" will load that file as original board
3. enter file name include suffix "-play" will load that file as game progress and file name exclude suffix "-play" as original board
4. save will save file name include suffix "-play" automatically