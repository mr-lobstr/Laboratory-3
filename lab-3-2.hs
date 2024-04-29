import System.Random
import Data.List

 -- решето Эратосфена
sieveEratosthenes [] = []
sieveEratosthenes (x:xs) = 
        x : sieveEratosthenes notMultiplies
    where notMultiplies = filter (\num -> num `mod` x /= 0) xs


-- простые числа до n
tablePrimes n = 
        if n >= 2
        then sieveEratosthenes [2 .. n]
        else []


-- возведенение в степень по модулю
powMod _ 0 _  = 1
powMod a power n =
        if even power
        then result
        else result * a `mod` n

    where newA = a * a `mod` n
          newPower = power `div` 2
          result = powMod newA newPower n


testMiller :: [Integer] -> Integer -> Int -> Bool
testMiller primeDivisors n t
    | n == 2 = True
    | n `mod` 2 == 0 || n < 2 = False
    | otherwise = all (\aRnd -> powMod aRnd (n - 1) n == 1) randNumbers &&
                  not (any (\q -> all (\aRnd -> powMod aRnd ((n - 1) `div` q) n == 1) randNumbers) primeDivisors)
    where
        randNumbers = take t $ randomRs (1, n-1) (mkStdGen 42)

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod x y m
    | even y = (powMod x (y `div` 2) m ^ 2) `mod` m
    | otherwise = (x * powMod x (y - 1) m) `mod` m


testPocklington :: [Integer] -> Integer -> Int -> Bool
testPocklington primeDivisorsF n t
    | n == 2 = True
    | n `mod` 2 == 0 || n < 2 = False
    | otherwise = all (\aRnd -> 
        let pred q = powMod aRnd ((n - 1) `div` q) n == 1
        in if powMod aRnd (n - 1) n /= 1
            then False
            else not $ any (\q -> pred q) primeDivisorsF
        ) randNumbers
    where
        randNumbers = take t $ randomRs (1, n-1) (mkStdGen 42)
        powMod base exp m = base ^ exp `mod` m


testProbability :: Integer -> Int -> IO Bool
testProbability n k
    | n == 2 = return True
    | n `mod` 2 == 0 || n < 2 = return False
    | otherwise = do
        let (t, s) = findTS (n - 1) 0
        testLoop n k t s

findTS :: Integer -> Int -> (Integer, Int)
findTS t s
    | t `mod` 2 == 0 = findTS (t `div` 2) (s + 1)
    | otherwise = (t, s)

testLoop :: Integer -> Int -> Integer -> Int -> IO Bool
testLoop n k t s
    | k <= 0 = return True
    | otherwise = do
        a <- randomRIO (2, n - 2)
        let x = powMod a t n
        if x == 1 || x == n - 1
            then testLoop n (k - 1) t s
            else do
                let result = innerLoop x n s
                if result
                    then testLoop n (k - 1) t s
                    else return False

innerLoop :: Integer -> Integer -> Int -> Bool
innerLoop x n s
    | s <= 0 = False
    | x == 1 || x == n - 1 = True
    | otherwise = innerLoop (powMod x 2 n) n (s - 1)

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod a b m = (a * powMod a (b - 1) m) `mod` m


randNumFactorisation :: [Integer] -> Int -> IO (Integer, [Integer])
randNumFactorisation primes bitSize = do
    gen <- newStdGen
    let maxFactor = (2 ^ bitSize) - 1
    let composite = 1
    let divisors = []
    let (prime, newGen) = randPrimeTo primes maxFactor gen
    let (factor, newGen') = randFromTo 1 5 newGen
    let updatedDivisors = nub (prime : divisors)
    let newFactor = prime ^ factor

    let updatedComposite = composite * newFactor
    let updatedMaxFactor = maxFactor `div` newFactor

    return (updatedComposite, updatedDivisors)


procedureMiller :: [Integer] -> Int -> Int -> IO (Integer, Int)
procedureMiller primes bitSize t = do
    let loop count = do
        (m, primeDivisors) <- randNumFactorisation primes (bitSize - 1)
        let n = 2 * m + 1
        let primeDivisors' = if m `mod` 2 == 0 then primeDivisors ++ [2] else primeDivisors
        if testMiller primeDivisors' n 200
            then return (n, count)
            else if testProbability n 6
                then loop (count + 1)
                else loop count
    loop 0


procedurePocklington :: [Integer] -> Int -> Int -> IO (Integer, Int)
procedurePocklington primes bitSize t = do
    gen <- getStdGen
    let (R, newGen) = randomR (2^(bitSize `div` 2), 2^((bitSize `div` 2) + 1) - 1) gen
        R' = if R `mod` 2 == 0 then R else R - 1
        (F, primeDivisors) = randNumFactorisation primes (bitSize `div` 2 + 1) newGen
        n = R' * F + 1
    if testPocklington primeDivisors n t
        then return (n, 0)
        else do
            let count = if testProbability n 6 then 1 else 0
            (result, finalCount) <- procedurePocklington primes bitSize t
            return (result, count + finalCount)


procedureGost :: Integer -> Int -> IO Double -> IO Integer
procedureGost q bitSize rand_01 = do
    let loop = do
        u <- return 0
        common <- return $ (2.0 ** fromIntegral (bitSize - 1)) / fromIntegral q
        N <- liftM2 (+) (return $ ceiling common) (liftM (* common) rand_01)
        let N' = if N `mod` 2 == 0 then N else N + 1
        let innerLoop u = do
            let p = (N' + u) * q + 1
            if p > 2 ^ bitSize then return ()
            else if (powMod 2 (p - 1) p == 1) && (powMod 2 (N' + u) p /= 1) then return p
            else innerLoop (u + 2)
        innerLoop u
        loop
    loop

powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod x y p
    | even y = (powMod x (y `div` 2) p) ^ 2 `mod` p
    | otherwise = (x * powMod x (y - 1) p) `mod` p

import Text.Printf

main :: IO ()
main = do
    let primes500 = tablePrimes 500
        bitSize = 13
        t = 5
        count = 10
        numsLen = length $ show (2 ^ bitSize)
    
    mapM_ (\i -> do
        (num, k) <- procedureMiller primes500 bitSize t
        let testProbRes = if testProbability num 6 then '+' else '-'
        printf "%2d) %*d %c %d\n" (i + 1) numsLen num testProbRes k
        ) [0..count-1]