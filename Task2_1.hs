module Task2_1 where

import Todo(todo)
import Prelude hiding (lookup)

-- Ассоциативный массив на основе бинарного дерева поиска
-- Ключи - Integer, значения - произвольного типа
data TreeMap v = Empty | Node { key   :: Integer,
                                value :: v,
                                left  :: TreeMap v,
                                right :: TreeMap v }
                                deriving (Show)

-- Пустое дерево
emptyTree :: TreeMap v
emptyTree = Empty

-- Содержится ли заданный ключ в дереве?
contains :: TreeMap v -> Integer -> Bool
contains Empty k = False
contains (Node key _ left right) k
    | k == key = True
    | k < key = contains left k
    | otherwise = contains right k

-- Значение для заданного ключа
lookup :: Integer -> TreeMap v -> v
lookup k Empty = error "No such key"
lookup k (Node key value left right)
    | k == key = value
    | k < key = lookup k left
    | otherwise = lookup k right

-- Вставка пары (ключ, значение) в деревопр
insert :: (Integer, v) -> TreeMap v -> TreeMap v
insert (k, v) Empty = Node k v Empty Empty
insert (k, v) (Node key value left right)
    | k == key = (Node key v left right)
    | k < key = (Node key value (newSubtree left) right)
    | otherwise = (Node key value left (newSubtree right))
        where newSubtree = insert (k, v)

-- Удаление элемента по ключу
remove :: Integer -> TreeMap v -> TreeMap v
remove i Empty = Empty
remove i (Node k v l r)
    | i == k = case (l, r) of
        (_, Empty) -> l
        (Empty, _) -> r
        _ -> Node (key minNode) (value minNode) l resid
                where
                residAndMin = removeAndGetMin r
                resid = fst residAndMin
                minNode = snd residAndMin
    | i < k = (Node k v (newSubtree l) r)
    | otherwise = (Node k v l (newSubtree r))
        where newSubtree = remove i

removeAndGetMin :: TreeMap v -> (TreeMap v, TreeMap v)
removeAndGetMin (Node k v l r) = case (left l) of
    Empty -> (Node k v (right l) r, l)
    _ -> ((Node k v (fst $ removeAndGetMin l) r), Empty)

-- Поиск ближайшего снизу ключа относительно заданного
nearestLE :: Integer -> TreeMap v -> (Integer, v)
nearestLE i Empty = error "Empty tree"
nearestLE i t = case tmp of
    Empty -> error "Lowest key value"
    _
        | key tmp == i -> (key tmp, value tmp)
        | otherwise -> (key resultNode, value resultNode)
            where 
            resultNode = case left tmp of
                Empty -> tmp
                _ -> maxNode (left tmp)
    where
    tmp = nearestLE' i t Empty

nearestLE' :: Integer -> TreeMap v -> TreeMap v -> TreeMap v
nearestLE' i Empty rightChosen = rightChosen
nearestLE' i t@(Node k _ l r) rightChosen
    | i == k = t
    | i < k = nearestLE' i l rightChosen
    | otherwise = nearestLE' i r t

maxNode :: TreeMap v -> TreeMap v
maxNode Empty = Empty
maxNode t@(Node _ _ _ Empty) = t
maxNode t = maxNode $ right t

-- Построение дерева из списка пар
treeFromList :: [(Integer, v)] -> TreeMap v
treeFromList [] = Empty
treeFromList (x:xs) = insert x $ treeFromList xs

-- Построение списка пар из дерева
listFromTree :: TreeMap v -> [(Integer, v)]
listFromTree Empty = []
listFromTree (Node k v l r) = 
    (k, v):((listFromTree l) ++ (listFromTree r))

-- Поиск k-той порядковой статистики дерева
size :: TreeMap v -> Integer
size Empty = 0
size t = (size $ left t) + (size $ right t) + 1
kMean :: Integer -> TreeMap v -> (Integer, v)
kMean _ Empty = error "No such rank"
kMean i (Node k v l r)
    | i == size l = (k, v)
    | i < size l = kMean i l
    | otherwise = kMean (i - size l - 1) r
