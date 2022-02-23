module Utils
  ( addToLast
  , whenSumOf
  , splitWhen
  , splitList
  , replaceAt
  , replaceAtBy
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

--sizedRows :: [[a]] -> [[(Int, a)]]
--sizedRows r = map sized r

-- we should be able to recurse over spans
-- - calculate the sizes of the row segment parts
-- - span with the wanted size
-- - if there are snd span, process that as new segment

withSumSizes :: [b] -> [(Int, b)]
withSumSizes xs = scanl1 (\(sb,b) (sa,a) -> (sb + sa, a)) (sized xs)

wss = withSumSizes (head rows)
-- [(2,1),(4,2),(6,3),(8,4)]

spanLessThan :: Ord a => a -> [(a, b)] -> ([(a, b)], [(a, b)])
spanLessThan n xs = span (\(x,_) -> x < n) xs

slt = spanLessThan 5 wss
-- ([(2,1),(4,2)],[(6,3),(8,4)])
slt1 = spanLessThan 1 wss
-- ([],[(2,1),(4,2),(6,3),(8,4)])

-- It becomes messy when the acc size is in a tuple with the elements
--  - see what happens if we keep the acc size outside the list of elements
--splitLine2 :: ([Integer] -> [(Int, Integer)]) -> Int -> [Integer] -> [[Integer]]

splitList2 :: (Ord b, Eq a) => ([a] -> [(b, a)]) -> b -> [a] -> [[a]]
splitList2 f n [] = []
splitList2 f n xs = let (done, todo) = spanLessThan n $ f xs
                        doneWOSize   = map snd done
                        todoWOSize   = map snd todo
                    in if doneWOSize == []
                       then []:[todoWOSize]
                       else doneWOSize:(splitList2 f n todoWOSize)

x = splitList2 sized 5 (head rows)
y = splitList2 sized 1 (head rows)

replaceIn :: [a] -> a -> [a] -> [a]
replaceIn pre x []       = pre ++ [x]
replaceIn pre x (_:post) = pre ++ [x] ++ post

replaceAt :: Int -> [a] -> a -> [a]
replaceAt i xs x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: Int -> [a] -> (a -> a) -> [a]
replaceAtBy i xs f = replaceAt i xs (f (xs !! max 0 i))
