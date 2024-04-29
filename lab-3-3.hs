coffee :: Double -> Double -> Double -> Int -> [(Double, Double)]
coffee Tsr Tc coefHeat t = take t $ iterate (\(i, temp) -> (i+1, temp - coefHeat * (temp - Tsr))) (0, Tc)

aprox :: [(Double, Double)] -> (Double, Double)
aprox coffeeInTime = (a, b)
  where
    n = fromIntegral $ length coffeeInTime
    sumXY = sum [x * y | (x, y) <- coffeeInTime]
    sumX = sum [x | (x, _) <- coffeeInTime]
    sumY = sum [y | (_, y) <- coffeeInTime]
    sumSqrX = sum [x^2 | (x, _) <- coffeeInTime]
    a = (n * sumXY - sumX * sumY) / (n * sumSqrX - sumX^2)
    b = (sumY - a * sumX) / n

korrel :: [(Double, Double)] -> Double
korrel coffeeInTime = sumDxDy / sqrt (sumSqrDx * sumSqrDy)
  where
    n = fromIntegral $ length coffeeInTime
    (mediumX, mediumY) = foldl (\(mx, my) (x, y) -> (mx + x, my + y)) (0, 0) coffeeInTime
    (sumDxDy, sumSqrDx, sumSqrDy) = foldl (\(sxy, sx, sy) (x, y) -> (sxy + (x - mediumX) * (y - mediumY), sx + (x - mediumX)^2, sy + (y - mediumY)^2)) (0, 0, 0) coffeeInTime

main :: IO ()
main = do
    putStrLn "Enter Tsr, Tc, coefHeat, and t:"
    [Tsr, Tc, coefHeat, t] <- fmap (map read . words) getLine
    let coffeeInTime = coffee Tsr Tc coefHeat t
        (a, b) = aprox coffeeInTime
        k = korrel coffeeInTime

    putStrLn "Time\tTemperature"
    mapM_ (\(time, temperature) -> putStrLn $ show time ++ "\t" ++ show temperature) coffeeInTime

    putStrLn $ "\nLine of approximation: T = " ++ show a ++ " * t + " ++ show b
    putStrLn $ "\nCorrelation coefficient: " ++ show k 
