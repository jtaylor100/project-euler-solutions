module Problems (problem_5, problem_6, problem_7, problem_8,problem_9) where

    import Data.Char
    import Data.List
    import Data.Function

    -- | Smallest positive number that can be divided  by each of the numbers
    -- | from 1..x without a remainder
    problem_5 :: Int -> Int
    problem_5 x = cleanlyDivisible numberRange 1
        where numberRange = [1..x]

    cleanlyDivisible :: [Int] -> Int -> Int
    cleanlyDivisible range x =
        if (filter (\a -> x `mod` a /= 0) range) == [] then x
        else cleanlyDivisible range (x + 1)
             
    -- | Difference between the sum of squares for [1..n]
    -- | e.g. 1^2 + 2^2 + .. + n^2
    -- | and the sqaure of sums for [1..n]
    -- | e.g. (1 + 2 + ... + n)^2
    problem_6 :: Int -> Int
    problem_6 n = abs (sumOfSquares - squareOfSums)
        where sumOfSquares = sum [x^ (2 :: Int) | x <- [1..n]]
              squareOfSums = (sum [1..n]) ^ (2 :: Int)

    -- | Returns the nth prime number
    problem_7 :: Int -> Int
    problem_7 n = primes !! n

    primes :: [Int]
    primes = [x| x <- [1..] , isPrime x]

    isPrime :: Int -> Bool
    isPrime x = filter (\a -> x `mod` a == 0) [2..primeLimit] == []
        where primeLimit =  round(sqrt(fromIntegral x) :: Double)

    -- | In thousandDigitNo, find the largest product created from the n
    -- | adjacent digits and return the adjacent digits
    problem_8 :: Int -> ([Integer],Integer)
    problem_8 n = maximumBy (compare `on` snd) $ listOfDigitsAndProducts n

    listOfDigitsAndProducts :: Int -> [([Integer], Integer)]
    listOfDigitsAndProducts n =
        let stringCombos = stringCombinations (show thousandDigitNo) n 
            digitCombos  = map (map  (toInteger . digitToInt)) stringCombos in
        zip digitCombos $ map product digitCombos

    stringCombinations :: String -> Int -> [String]
    stringCombinations str n = [ take n $ drop x str | x <- [0..(1000 - n)]]
                           
    thousandDigitNo :: Integer                        
    thousandDigitNo = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

    -- | For the inputted x, find a combination of numbers (y1,y2,y3) that sum
    -- | to x and satisfy the conditions:
    -- * y1 < y2 < y3
    -- * y1^2 + y2^2 = y3^2
    -- | Returning the product of (y1,y2,y3)
    problem_9 :: Int -> Int
    problem_9 x = product $ listed
                  where listed = y1 : y2 : y3 : []
                        (y1,y2,y3) = head $ [(y1,y2,y3) |
                                          y1 <- [1..(x `div` 2)],
                                          y2 <- [1..y1],
                                          let c = x - y1 - y2,
                                          y1^2 + y2^2 == c^2]
