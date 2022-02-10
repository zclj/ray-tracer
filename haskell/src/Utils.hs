module Utils
  ( addToLast
  , whenSumOf
  , splitWhen
  , splitList
  , replaceAt
  , replaceAtBy
  ) where

addToLast :: a -> [[a]] -> [[a]]
addToLast x xs = pre ++ [post ++ [x]]
  where pre  = init xs
        post = last xs

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

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = replaceIn pre x post
  where (pre, post) = splitAt i xs

replaceAtBy :: [a] -> Int -> (a -> a) -> [a]
replaceAtBy xs i f = replaceAt xs i (f (head y))
  where (_, y) = splitAt i xs  
