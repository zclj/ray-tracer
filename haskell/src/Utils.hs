module Utils
  ( addToLast
  , whenSumOf
  , splitWhen
  , splitList
  , replaceAt
  , replaceAtBy
  ) where

addToLast :: a -> [[a]] -> [[a]]
addToLast x xs = concat [init xs, [concat [last xs, [x]]]]

whenSumOf :: (Num b, Ord b) => (b -> Bool) -> (a -> b) -> [a] -> Bool
whenSumOf p f l = p $ sum (map f l)

splitWhenR :: ([a] -> Bool) -> [a] -> [[a]] -> [[a]]
splitWhenR p [] acc        = acc
splitWhenR p r@(x:xs) acc  = if p (last (addToLast x acc))
                             then splitWhenR p r (acc ++ [[]])
                             else splitWhenR p xs (addToLast x acc)

splitWhen :: ([a] -> Bool) -> [a] -> [[a]]
splitWhen p xs = splitWhenR p xs [[]]                                  

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
