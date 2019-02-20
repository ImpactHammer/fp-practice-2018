module Task2_2 where

import Todo(todo)

import Prelude hiding (foldl, foldr, unfoldr, map, concatMap, 
    filter, maxBy, minBy, reverse, sum, product, elem)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f b [] = b
foldl f b (x:xs) = foldl f (f b x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b = case f b of
    Nothing -> []
    Just (a0, b0) -> a0:(unfoldr f b0)

-- Сумма всех элементов списка (пример)
sum :: [Integer] -> Integer
sum lst = foldl (+) 0 lst

-- Переворот списка (Пример)
reverse :: [a] -> [a]
reverse lst = foldl f [] lst where f t h = h:t

-- Отображение элементов списка
map :: (a -> b) -> [a] -> [b]
map f lst = foldr f0 [] lst where f0 h t = (f h):t

-- Произведение всех элементов списка
product :: [Integer] -> Integer
product lst = foldr (*) 1 lst

-- Выделение из списка Maybe всех существующих значений
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes lst = foldr f [] lst
    where 
    f x xs = case x of
        Nothing -> xs
        Just y -> y:xs

-- Диагональ матрицы
diagonal :: [[a]] -> [a]
diagonal lst = zipWith (!!) lst [0..(length $ head lst) - 1]

-- Фильтр для всех элементов, не соответствующих предикату
filterNot :: (a -> Bool) -> [a] -> [a]
filterNot p lst = foldr f [] lst 
    where 
    f x xs
        | p x = xs
        | otherwise = x:xs

-- Поиск элемента в списке
elem :: (Eq a) => a -> [a] -> Bool
elem e lst = foldr f False lst
    where
    f x xs = (x == e) || xs

-- Список чисел в диапазоне [from, to) с шагом step
rangeTo :: Integer -> Integer -> Integer -> [Integer]
rangeTo from to step = unfoldr f from
    where
    f b
        | b < to = Just (b, b + step)
        | otherwise = Nothing

-- Конкатенация двух списков
append :: [a] -> [a] -> [a]
append a b = foldr f b a
    where
    f x xs = x:xs

-- Разбиение списка lst на куски размером n
-- (последний кусок может быть меньше)
groups :: [a] -> Integer -> [[a]]
groups lst n
    | (n < 1) || (n > toInteger (length lst)) = error "Invalid group size"
    | otherwise = reverse $ result
        where
        result = case (fst tpl) of 
            [] -> snd tpl
            _ -> (fst tpl):(snd tpl)
        tpl = foldl f ([], []) lst
        f (l, ll) x
            | toInteger (length (x:l)) == n = ([], (reverse (x:l)):ll)
            | otherwise = ((x:l), ll)
