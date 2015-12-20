module Problems () where

    import Data.Char
    import Data.List
    import Data.Function
    import Data.String.Utils
    import Math.NumberTheory.Primes.Factorisation

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
        where doubleListOfStrings = map (split " ") $ lines gridOfNumbers    

    gridOfNumbers :: String
    gridOfNumbers = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n\
                    \49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n\
                    \81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n\
                    \52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n\
                    \22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n\
                    \24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n\
                    \32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n\
                    \67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n\
                    \24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n\
                    \21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n\
                    \78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n\
                    \16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n\
                    \86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n\
                    \19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n\
                    \04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n\
                    \88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n\
                    \04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n\
                    \20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n\
                    \20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n\
                    \01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"

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
