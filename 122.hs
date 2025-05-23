     --Exercice 1
 
-- Affichage du PPCM
affiche :: Integer -> Integer -> String
affiche a b = "Le ppcm de " ++ show a ++ " et " ++ show b ++ " est = "
-- 1)
pgcd :: Integer -> Integer -> Integer
pgcd a b
    | a == b    = a
    | a > b     = pgcd (a - b) b
    | otherwise = pgcd a (b - a)

--2)
ppcm :: Integer -> Integer -> String
ppcm a b = affiche a b ++ show (abs (a * b) `div` pgcd a b)

--3)
sommeimpairs:: Int->[Int]
sommeimpairs x= filter(\x->x`mod`2/=0) [1..x]

--4)

divMod' :: Int -> Int -> (Int, Int)
divMod' a b = (a `mod` b, a `div` b)

--5)
myMin:: Integer->Integer->Integer
myMin a b
      |a==b      = error "Entrer deux nombres différent"
      |a>b       =b
      |otherwise =a

myMax:: Integer->Integer->Integer      
myMax a b 
      |a==b      = error "Entrer deux nombres différent" 
      |a>b       =a 
      |otherwise =b

--6)
minimum':: Integer->Integer->Integer->Integer->Integer
minimum' a b c d= minimum[a,b,c,d]
maximum':: Integer->Integer->Integer->Integer->Integer
maximum' a b c d= maximum[a,b,c,d]          

--7)
bornerDans :: Integer -> Integer -> Integer -> Integer
bornerDans a b x = myMax (myMin a b) (myMin (myMax a b) x)

--8)

sommeChiffres :: Int -> Int
sommeChiffres 0 = 0
sommeChiffres n = (n `mod` 10) +  sommeChiffres(n `div` 10 )

--9)
isPrime :: Int -> Bool
isPrime n
    | n < 2     = False
    | n == 2    = True
    | even n    = False
    | otherwise = all (\x -> n `mod` x /= 0) [3,5..floor (sqrt (fromIntegral n))]



-- 10)
distance :: (Float, Float) -> (Float, Float) -> Float
distance (a1, b1) (a2, b2) = sqrt ((a2 - a1)^2 + (b2 - b1)^2)

--11) 
u :: Int -> Int
u 0 = -2
u n = 3 + 4 * u (n - 1)



--12)

sommeEntiers :: Int -> Int
sommeEntiers n = n * (n + 1) `div` 2
sommeCarres :: Int -> Int
sommeCarres n = n * (n + 1) * (2 * n + 1) `div` 6
sommeHarmonique :: Int -> Double
sommeHarmonique n = sum [1 / fromIntegral k | k <- [1..n]]
sommeAlternee :: Int -> Double
sommeAlternee n = sum [((-1) ** fromIntegral (k - 1)) / fromIntegral k | k <- [1..n]]
sommeInverseCarres :: Int -> Double
sommeInverseCarres n = sum [1 / fromIntegral (k ^ 2) | k <- [1..n]]

    --Exercice 2 et 3

data Jours = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche 
    deriving(Show, Enum)
est Samedi = "Week-end"
est Dimanche = "Week-end"
est _ = "Jour de travail" 

numeroJour :: Jours -> Int
numeroJour jour = fromEnum jour + 1

data Resultat = Singleton Double | Couple (Double, Double)
    deriving (Show)

snddegres :: Double -> Double -> Double -> Resultat
snddegres a b c
    | delta < 0  = error "Pas de solution réelle"
    | delta == 0 = Singleton (-b / (2 * a))
    | otherwise  = Couple ((-b - sqrt delta) / (2 * a), (-b + sqrt delta) / (2 * a))
  where delta = b^2 - 4*a*c


--Exercice 4

type Couple = (Integer, Integer)

exo :: Couple -> Couple -> Integer
exo (a, b) (c, d) = a * c + b * d


    
