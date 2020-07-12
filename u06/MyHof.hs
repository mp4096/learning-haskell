zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []       []       = []
zipWith' _ _        []       = []
zipWith' _ []       _        = []
zipWith' f (x : xs) (y : ys) = [f x y] ++ (zipWith' f xs ys)

unzipWith :: (t -> (a, b)) -> [t] -> ([a], [b])
unzipWith _ [] = ([], [])
unzipWith f xs =
  let list_of_tuples = map f xs
  in  ([ fst t | t <- list_of_tuples ], [ snd t | t <- list_of_tuples ])

main :: IO ()
main = do
  let a = [1 ..] :: [Int]
      b = [1, 10, 100] :: [Int]
      c = [(0, 'g'), (8, 'u'), (9, 't')] :: [(Int, Char)]
      d = [(0, 1), (8, 2), (9, 3)] :: [(Int, Int)]
  print (zipWith' (+) a b)
  print (zipWith' (*) a b)
  print (zipWith' (\x y -> (x, y)) a b)
  print (unzipWith id c)
  print (unzipWith (\(a, b) -> (a + b, a * b)) d)
