any' :: (a -> Bool) -> [a] -> Bool
any' _ []       = False
any' f (x : xs) = (f x) || (any' f xs)


all' :: (a -> Bool) -> [a] -> Bool
all' _ []       = True
all' f (x : xs) = (f x) && (all' f xs)

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = foldr (:) (map' f xs) [f (x)]

main :: IO ()
main = do
  let a = [1, 2, 3, 4] :: [Int]
      b = [1, 1, 1, 1] :: [Int]
  print (any' ((==) 1) a)
  print (all' ((==) 1) b)
  print (any' ((==) 5) a)
  print (all' ((==) 5) b)
  print (map' ((*) 10) a)
