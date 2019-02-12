module Task1_2 where

import Todo(todo)
import Prelude hiding (gcd, sin)

-- синус числа (формула Тейлора)
sin :: Double -> Double
sin x = sin' x (x**3) (3 :: Double) (6 :: Double) x
    where
    sin' :: Double -> Double -> Double -> Double -> Double -> Double
    sin' x x_pow n n_fact result
        | (abs sin_entry) > 0 = sin'
        x (x_pow * x * x) (n + 2) (n_fact * (n + 2) * (n + 1)) (result + sin_entry)
        | otherwise = result
        where
        sin_entry = (-1)**((n - 1) / 2) * x_pow / n_fact

-- косинус числа (формула Тейлора)
cos :: Double -> Double
cos x = todo

-- наибольший общий делитель двух чисел
gcd :: Integer -> Integer -> Integer
gcd x y
    | y == 0 = x
    | otherwise = gcd y (x `mod` y)

-- существует ли полный целочисленный квадрат в диапазоне [from, to)?
doesSquareBetweenExist :: Integer -> Integer -> Bool
doesSquareBetweenExist from to = bound (to - 1) - bound from > 0
    where
    bound = floor . sqrt . fromIntegral

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = todo

-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
pow :: Integer -> Integer -> Integer
pow x y
    | y > 0 = pow' 1 x y
    | otherwise = 0
    where
    pow' prod _ 0 = prod
    pow' prod x y = pow' (prod * x) x (y - 1)

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime x = isPrime' x 2 (ceiling . sqrt . fromIntegral $ x)
    where
    isPrime' :: Integer -> Integer -> Integer -> Bool
    isPrime' x n last
        | n == last = (x `mod` last) /= 0
        | (x `mod` n) == 0 = False
        | otherwise = isPrime' x (n + 1) last

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
shapeArea points = todo

-- треугольник задан своими координатами.
-- функция должна вернуть 
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Point2D -> Point2D -> Point2D -> Integer
triangleKind a b c = todo