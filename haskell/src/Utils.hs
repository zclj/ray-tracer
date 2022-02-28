module Utils
  ( addToLast
  , whenSumOf
  , splitWhen
  , splitList
  , splitList2
  , splitList3
  , replaceAt
  , replaceAtBy
  , LineSegment (..)
  ) where

addToLast :: a -> [[a]] -> [[a]]
addToLast x [] = [[x]]
addToLast x (y:ys) = let a = x:y
                     in a:ys

whenSumOf :: (Num b, Ord b) => (b -> Bool) -> (a -> b) -> [a] -> Bool
whenSumOf p f l = p $ sum (map f l)

splitWhenR :: ([a] -> Bool) -> [a] -> [[a]] -> [[a]]
splitWhenR p [] acc        = acc
splitWhenR p r@(x:xs) acc  = if p (head (addToLast x acc))
                             then splitWhenR p r  ([]:acc)
                             else splitWhenR p xs (addToLast x acc)

splitWhen :: ([a] -> Bool) -> [a] -> [[a]]
splitWhen p xs = reverse $ map reverse (splitWhenR p xs [[]])

{-|
  'splitList' split a list at the points where the 'size' of the items in the list
  reaches 'n'. The 'size' of the list is determined by f.
-}
splitList :: (Num b, Ord b) => [a] -> b -> (a -> b) -> [[a]]
splitList r n f =
  splitWhen (\l -> whenSumOf (> (n - fromIntegral (length l))) f l) r

----------------------------------------
-- recursive split

rows = [[1,2,3,4],[5,6,7,8,9]]

sized :: [a] -> [(Int, a)]
sized r = map (\x -> (2,x)) r

withSumSizes :: Num a => ([b] -> [(a, b)]) -> [b] -> [(a, b)]
withSumSizes f xs = scanl1 (\(sb,b) (sa,a) -> (sb + sa, a)) (f xs)

wss = withSumSizes sized (head rows)
-- [(2,1),(4,2),(6,3),(8,4)]

-- aggregate 'length' (the number of items in the list),
--  'size' (the space the item takes)
--  and the item itself
data LineSegment a = LineSegment { segmentLength :: Int
                                 , segmentSpace  :: Int
                                 , segmentValue  :: a }
                     deriving (Show)

testSegment = LineSegment 1 2 "thing"
testSegment2 = LineSegment 1 2 4
testSegment3 = LineSegment 1 2 "thing2"

--segments1 = [testSegment, testSegment2] => This will not compile
segments2 = [testSegment, testSegment3]

sized2 :: [a] -> [LineSegment a]
sized2 r = map (\x -> (LineSegment 1 2 x)) r

-- withSegments :: Num a => ([b] -> [(a, a, b)]) -> [b] -> [(a, a, b)]
-- withSegments f xs = scanl1 (\(sb,b,x) (sa,a,y) -> (sb + sa, b+a, y)) (f xs)

withSegments :: ([b] -> [LineSegment b]) -> [b] -> [LineSegment b]
withSegments f xs = scanl1 (\(LineSegment sb b _) (LineSegment sa a y) ->
                              (LineSegment (sb + sa) (b + a) y)) (f xs)

wseg = withSegments sized2 (head rows)
-- [LineSegment {segmentLength = 1, segmentSpace = 2, segmentValue = 1},LineSegment {segmentLength = 2, segmentSpace = 4, segmentValue = 2},LineSegment {segmentLength = 3, segmentSpace = 6, segmentValue = 3},LineSegment {segmentLength = 4, segmentSpace = 8, segmentValue = 4}]

spanLessThan :: Ord a => a -> [(a, b)] -> ([(a, b)], [(a, b)])
spanLessThan n xs = span (\(x,_) -> x < n) xs

slt = spanLessThan 5 wss
-- ([(2,1),(4,2)],[(6,3),(8,4)])
slt1 = spanLessThan 1 wss
-- ([],[(2,1),(4,2),(6,3),(8,4)])

spanLessThan2 :: Int -> [LineSegment b] -> ([LineSegment b], [LineSegment b])
spanLessThan2 n xs = span (\(LineSegment length space _) -> (length+space) < n) xs

slt3 = spanLessThan2 5 wseg
-- ([LineSegment {segmentLength = 1, segmentSpace = 2, segmentValue = 1}],[LineSegment {segmentLength = 2, segmentSpace = 4, segmentValue = 2},LineSegment {segmentLength = 3, segmentSpace = 6, segmentValue = 3},LineSegment {segmentLength = 4, segmentSpace = 8, segmentValue = 4}])
slt4 = spanLessThan2 1 wseg
-- ([],[LineSegment {segmentLength = 1, segmentSpace = 2, segmentValue = 1},LineSegment {segmentLength = 2, segmentSpace = 4, segmentValue = 2},LineSegment {segmentLength = 3, segmentSpace = 6, segmentValue = 3},LineSegment {segmentLength = 4, segmentSpace = 8, segmentValue = 4}])

-- The 'client' size f, in the case of ppm, need both the aggregated 'space' taken
--  in total by the included samples, but also the aggregated length of the line segment.
--  The total ppm size is calculated with space+length, length beeing the required length
--   of white-space in the final output.
splitList2 :: (Ord a, Num a, Eq b) => [b] -> a -> ([b] -> [(a, b)]) -> [[b]]
splitList2 [] n f = []
splitList2 xs n f = let (done, todo) = spanLessThan n $ withSumSizes f xs
                        doneWOSize   = map snd done
                        todoWOSize   = map snd todo
                    in if doneWOSize == []
                       then []:[todoWOSize]
                       else doneWOSize:(splitList2 todoWOSize n f)

x = splitList2 (head rows) 5 sized
y = splitList2 (head rows) 1 sized

splitList3 :: (Eq b) => [b] -> Int -> ([b] -> [LineSegment b]) -> [[b]]
splitList3 [] n f = []
splitList3 xs n f = let (done, todo) = spanLessThan2 n $ withSegments f xs
                        doneWOSize   = map segmentValue done
                        todoWOSize   = map segmentValue todo
                    in if doneWOSize == []
                       then []:[todoWOSize]
                       else doneWOSize:(splitList3 todoWOSize n f)

z = splitList3 (head rows) 5 sized2

replaceIn :: [a] -> a -> [a] -> [a]
replaceIn pre x []       = pre ++ [x]
replaceIn pre x (_:post) = pre ++ [x] ++ post

replaceAt :: Int -> [a] -> a -> [a]
replaceAt i xs x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: Int -> [a] -> (a -> a) -> [a]
replaceAtBy i xs f = replaceAt i xs (f (xs !! max 0 i))
