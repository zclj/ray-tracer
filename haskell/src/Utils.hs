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

replaceIn :: [a] -> a -> [a] -> [a]
replaceIn pre x []       = pre ++ [x]
replaceIn pre x (_:post) = pre ++ [x] ++ post

replaceAt :: Int -> [a] -> a -> [a]
replaceAt i xs x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: Int -> [a] -> (a -> a) -> [a]
replaceAtBy i xs f = replaceAt i xs (f (xs !! max 0 i))
