data ML a = E | L a (ML a) deriving Show

myHead :: ML a -> a
myHead E       = error "empty list"
myHead (L x _) = x

myTail :: ML a -> ML a
myTail E        = error "empty list"
myTail (L _ xs) = xs

myAdd :: Num a => ML a -> ML a -> ML a
myAdd E        E        = E
myAdd E        (L _ _)  = E
myAdd (L _ _ ) E        = E
myAdd (L x xs) (L y ys) = L (x + y) (myAdd xs ys)

myAppend :: ML a -> ML a -> ML a
myAppend E        E     = E
myAppend E        right = right
myAppend left     E     = left
myAppend (L x xs) right = L x (myAppend xs right)

toString :: Show a => ML a -> String
toString E        = ""
toString (L x E ) = show x
toString (L x xs) = show x ++ ", " ++ toString xs

main :: IO ()
main = do
  let ml1234 = L 1 (L 2 (L 3 (L 4 E))) :: ML Int
      ml42   = L 42 E :: ML Int
  print ml1234
  print (myHead ml42)
  print (myTail ml1234)
  print (myAdd (myTail ml1234) ml1234)
  print (myAppend ml1234 ml42)
  print (toString ml1234)
