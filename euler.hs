module Problems () where

    -- Base packages
    import Data.Char
    import Data.List
    import Data.Function
    import Data.Ord

    -- MissingH
    import Data.String.Utils as S

    -- arithmoi
    import Math.NumberTheory.Primes.Factorisation

    -- split
    import Data.List.Split

    -- Local packages
    import LargeNumbers

    -- date
    import Data.Time
    import Data.Time.Calendar.WeekDate

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
    primes = [x| x <- [2..] , isPrime x]

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
                                          let y3 = x - y1 - y2,
                                          y1^2 + y2^2 == y3^2]

    -- | For the inputted x, find the sum of all of the primes below x
    problem_10 :: Int -> Int
    problem_10 x = sum $ takeWhile (< x) primes 


    -- | In the `gridOfNumbers` find the 4 adjacent numbers in the given
    -- | direction that have the greatest sum.
    problem_11 :: Direction -> Int
    problem_11 dir =  
        maximum $ map (\(x1,x2,x3,x4) -> product $ x1:x2:x3:x4:[]) $ numberPairs dir

    data Direction = UpDown | SideWays | LeftStartDiagonal | RightStartDiagonal

    numberPairs :: Direction -> [(Int,Int,Int,Int)]
    numberPairs UpDown = 
        [(x1,x2,x3,x4) | 
         currentRow <- [0..16],
         currentCol <- [0..19],
         let x1 = (listifyGrid !! currentRow) !! currentCol,
         let x2 = (listifyGrid !! (currentRow + 1)) !! currentCol,
         let x3 = (listifyGrid !! (currentRow + 2)) !! currentCol,
         let x4 = (listifyGrid !! (currentRow + 3)) !! currentCol] 
    numberPairs SideWays =
        [(x1,x2,x3,x4) |
         currentRow <- [0..19],
         currentCol <- [0..16],
         let x1 = (listifyGrid !! currentRow) !! currentCol,
         let x2 = (listifyGrid !! currentRow) !! (currentCol + 1),
         let x3 = (listifyGrid !! currentRow) !! (currentCol + 2),
         let x4 = (listifyGrid !! currentRow) !! (currentCol + 3)]
    numberPairs LeftStartDiagonal =
        [(x1,x2,x3,x4) |
         currentRow <- [0..16],
         currentCol <- [0..16],
         let x1 = (listifyGrid !! currentRow) !! currentCol,
         let x2 = (listifyGrid !! (currentRow + 1)) !! (currentCol + 1),
         let x3 = (listifyGrid !! (currentRow + 2)) !! (currentCol + 2),
         let x4 = (listifyGrid !! (currentRow + 3)) !! (currentCol + 3)]
    numberPairs RightStartDiagonal =
        [(x1,x2,x3,x4) |
         currentRow <- [0..16],
         currentCol <- [3..19],
         let x1 = (listifyGrid !! currentRow) !! currentCol,
         let x2 = (listifyGrid !! (currentRow + 1)) !! (currentCol - 1),
         let x3 = (listifyGrid !! (currentRow + 2)) !! (currentCol - 2),
         let x4 = (listifyGrid !! (currentRow + 3)) !! (currentCol - 3)]

    
    listifyGrid :: [[Int]]
    listifyGrid = map (map read) doubleListOfStrings 
        where doubleListOfStrings = map (S.split " ") $ lines gridOfNumbers    

    -- | Find the triangle number that has x number of divisors
    problem_12 :: Int -> Int
    problem_12 x = 
        head $ dropWhile (\a -> not $ hasGtOrEqFactors a x) triangleNumbers

    hasGtOrEqFactors :: Int -> Int -> Bool
    hasGtOrEqFactors a factorCount = numberOfFactors a >= factorCount

    -- | Works out number of factors by prime factorisation,
    -- | 2^x + 3^y + ... = a , (x+1)*(y+1)*... = factor number
    numberOfFactors :: Int -> Int 
    numberOfFactors a = product $ map (\(_,x) -> x+1) (factorise $ toInteger a)

    triangleNumbers :: [Int]
    triangleNumbers = [ x | n <- [1..] , let x = sum [1..n]] 

    problem_13 :: Integer
    problem_13 = firstTenDigits (sum fiftyDigitNumbers)

    firstTenDigits :: Integer -> Integer
    firstTenDigits a = read $ take 10 (show a)

    fiftyDigitNumbers :: [Integer]
    fiftyDigitNumbers = map (\a -> read a) (lines rawNumbersString)

    problem_14 :: (Int, Int) 
    problem_14 = maximumBy (comparing snd) collatzSeqLengths 
        where collatzSeqLengths = [(n, colLength) |
                                   n <- [1..1000000], 
                                   let colLength = length (collatzSeq n)]
 
    collatzSeq :: Int -> [Int] 
    collatzSeq a
        | a == 1 = []
        | even a = [(a `quot` 2)] ++ (collatzSeq (a `quot` 2))
        | odd  a = [(3*a + 1)] ++ (collatzSeq (3*a + 1))


    -- | Given a square grid with outer-edge length of x, how many different
    -- | paths can take you from the top left vertex to bottom right?
    problem_15 :: Integer -> Integer
    problem_15 x = choose (2*x) x

    choose :: Integer -> Integer -> Integer
    choose x y = (factorial x) `div` ((factorial y) * (factorial (x - y))) 

    factorial :: Integer -> Integer
    factorial x = product [ xs | xs <- [1..x]]

    -- | Given a number x, find 2 ^ x and find the sum of its digits
    problem_16 :: Int -> Int
    problem_16 x = sum $ map digitToInt $ show (2^x)

    -- | Given a number x, build [1..x], write all of the numbers in words
    -- | count the characters used and return the count
    problem_17 :: Int -> Int
    problem_17 x = sum
                    $ map length
                    $ map (stripChars " -" )
                    $ map intToWord [1..x]

    stripChars :: String -> String -> String
    stripChars = filter . flip notElem

    intToWord :: Int -> String
    intToWord 1 = "one"
    intToWord 2 = "two"
    intToWord 3 = "three"
    intToWord 4 = "four"
    intToWord 5 = "five"
    intToWord 6 = "six"
    intToWord 7 = "seven"
    intToWord 8 = "eight"
    intToWord 9 = "nine"
    intToWord 10 = "ten"
    intToWord 11 = "eleven"
    intToWord 12 = "twelve"
    intToWord 13 = "thirteen"
    intToWord 15 = "fifteen"
    intToWord 18 = "eighteen"
    intToWord 20 = "twenty"
    intToWord 30 = "thirty"
    intToWord 40 = "forty"
    intToWord 50 = "fifty"
    intToWord 60 = "sixty"
    intToWord 70 = "seventy"
    intToWord 80 = "eighty"
    intToWord 90 = "ninety"
    intToWord 1000 = "one thousand"
    intToWord x
    -- TEEN numbers 13..19
        | x < 20 = intToWord (x - 10) ++ "teen"
    -- TENS numbers 21..29, 31..39, etc up to 99
        | x < 100 = let tensValue = (x `div` 10) * 10
                        unitValue = x - tensValue
                    in intToWord tensValue ++ "-" ++ intToWord unitValue
    -- HUNDREDS numbers 100..999
        | x `mod` 100 == 0 = intToWord (x `div` 100) ++ " hundred"
        | x < 1000 = let hundredsValue = x `div` 100
                         rest = x - hundredsValue * 100
                     in intToWord hundredsValue ++ " hundred and " ++ intToWord rest

    -- | For the number triangle "largeTriangle", find the greatest sum of
    -- | numbers that form a path from the apex to the triangle bottom,
    -- | going down a row each time
    problem_18 :: Int
    problem_18 = greatestPathSum $ parseTriangle biggerTriangle

    greatestPathSum :: [[Int]] -> Int
    greatestPathSum triangle = maximum $ map sum $ allPossiblePaths triangle

    allPossiblePaths :: [[Int]] -> [[Int]]
    allPossiblePaths triangle
        | (length triangle) == 1 = triangle
        | otherwise              = map (apex :) $ possiblePaths
        where apex          = triangle !! 0 !! 0
              subtriR       = getSubTriangle R triangle
              subtriL       = getSubTriangle L triangle
              possiblePaths = allPossiblePaths subtriR ++ allPossiblePaths subtriL

    data TriangleDir = L | R

    getSubTriangle :: TriangleDir -> [[Int]] -> [[Int]]
    getSubTriangle L tri =  delete [] $ map init tri
    getSubTriangle R tri =  delete [] $ map tail tri

    parseTriangle :: String -> [[Int]]
    parseTriangle str = map (map read) $ map (S.split " ") $ lines str

    -- | Find the number of Sundays in the 20th Century that fell as the first
    -- | of the month
    problem_19 :: Int
    problem_19 = firstMonthSundays (fromGregorian 1901 1 1) (fromGregorian 2000 12 31)

    firstMonthSundays :: Day -> Day -> Int
    firstMonthSundays x n = length $ filter isSunday $ filter isFirstOfMonth [x..n]

    isFirstOfMonth :: Day -> Bool
    isFirstOfMonth day
        | dayOfMonth == 1 = True
        | otherwise       = False
        where (_,_,dayOfMonth) = toGregorian day

    isSunday :: Day -> Bool
    isSunday day
        | weekDay == 7 = True
        | otherwise    = False
        where (_,_,weekDay) = toWeekDate day

    -- | Find x! and find the sum of its digits
    problem_20 :: Integer -> Int
    problem_20 x = sum $ map digitToInt $ show $ factorial x
