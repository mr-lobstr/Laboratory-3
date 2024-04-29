import Text.Printf
import Data.List

function :: Double -> Double
function x
    | x <= -2 = func1 x
    | -2 < x && x < 2 = func2 x
    | otherwise = func3 x

columnsWidth :: Int -> Double -> Double -> Int -> Int
columnsWidth titleWidth x0 x1 prec = maximum [titleWidth, x0Len, x1Len]
    where
        x0Len = length $ printf "%.*f" prec x0
        x1Len = length $ printf "%.*f" prec x1

main :: IO ()
main = do
    let x0 = 3.12345
        x1 = 6
        step = 0.1
        titleArg = "x:"
        titleRes = "results:"
        width = columnsWidth (length titleArg) x0 x1 3 + 1
    putStrLn "Table"
    putStrLn $ printf "%-*s%s" width titleArg titleRes
    forM_ [x0, x0 + step .. x1] $ \x -> do
        let newX = if abs x < step / 10 then 0 else x
        putStrLn $ printf "%-*.*f%.*f" width 3 newX (function newX)

func1 :: Double -> Double
func1 x = -(x + 3) / 2

func2 :: Double -> Double
func2 x = 2 * cos (2 * x)

func3 :: Double -> Double
func3 x = (x + 3) / 2
