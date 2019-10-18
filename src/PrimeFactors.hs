module PrimeFactors
  ( primes,
    isPrime,
    factors,
  )
where

factors :: Integer -> [Integer]
factors 0 = []
factors 1 = []
factors n = reverse $ go n 2 []
  where
    go 1 _ ps = ps
    go n p ps =
      if n `mod` p == 0
        then go (n `div` p) p (p : ps)
        else go n (p + 1) ps

sieve :: Int -> [Int]
sieve n = reverse $ go [2 .. n] []
  where
    go [] ps = ps
    go (p : ns) ps = go (filter ((/= 0) . (`mod` p)) ns) (p : ps)

isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n | n `mod` 2 == 0 = False
isPrime n = not . any ((== 0) . (n `mod`)) $ [3, 5 .. (ceiling . sqrt . fromIntegral $ n)]

primes :: [Integer]
primes = filter isPrime [2 ..]
