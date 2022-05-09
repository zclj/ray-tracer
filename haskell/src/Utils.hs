module Utils
  ( whenSumOf
  , splitList
  , replaceAt
  , replaceAtBy
  , dropAt
  ) where

whenSumOf :: (Num b, Ord b) => (b -> Bool) -> (a -> b) -> [a] -> Bool
whenSumOf p f l = p $ sum (map f l)

splitWhenR :: (Int -> [a] -> Bool) -> Int -> [a] -> [[a]] -> [[a]]
splitWhenR p l [] acc               = acc
splitWhenR p l r@(x:xs) acc@(y:ys)  = let nextList = ((x:y):ys)
                                      in if p l (head nextList)
                                         then splitWhenR p 1 r  ([]:acc)
                                         else splitWhenR p (l+1) xs nextList

splitWhen :: (Int -> [a] -> Bool) -> [a] -> [[a]]
splitWhen p xs = reverse $ map reverse (splitWhenR p 1 xs [[]])

{-|
  'splitList' split a list at the points where the 'size' of the items in the list
  reaches 'n'. The 'size' of the list is determined by f.
-}
splitList :: [a] -> Int -> (a -> Int) -> [[a]]
splitList r n f =
  splitWhen (\padding xs -> whenSumOf (> (n - padding)) f xs) r

replaceIn :: [a] -> a -> [a] -> [a]
replaceIn pre x []       = pre ++ [x]
replaceIn pre x (_:post) = pre ++ [x] ++ post

replaceAt :: Int -> [a] -> a -> [a]
replaceAt i xs x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: Int -> [a] -> (a -> a) -> [a]
replaceAtBy i xs f = replaceAt i xs (f (xs !! max 0 i))

-- dropAt :: Int -> [a] -> [a]
-- dropAt i xs
--   | i == 0               = tail xs
--   | i == (length xs) - 1 = init xs
--   | otherwise            = pre ++ tail post
--                            where (pre, post) = splitAt i xs

dropAt :: Int -> [a] -> [a]
dropAt i (x:y:z:w:_) = case i of
                         0 -> [y,z,w]
                         1 -> [x,z,w]
                         2 -> [x,y,w]
                         3 -> [x,y,z]
dropAt i (x:y:z:_) = case i of
                       0 -> [y,z]
                       1 -> [x,z]
                       2 -> [x,y]

