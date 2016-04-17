data Iterator a = a :< (Iterator a) deriving (Show)

infixr 5 :<

initialHistory :: Iterator String
initialHistory = "" :< initialHistory

exampleHistory :: Iterator String
exampleHistory =
     "hello"
  :< "help"
  :< "ls"
  :< initialHistory

next :: Iterator a -> a
next (_ :< (value :< _)) = value

extract :: Iterator a -> a
extract (value :< _) = value

extend :: (Iterator a -> b) -> Iterator a -> Iterator b
extend f ite@(_ :< tail) = (f ite) :< (extend f tail)

main :: IO ()
main =
  putStrLn $ show $ extract $ extend next $ extend next exampleHistory

------------------------------------

next' :: Iterator a -> Iterator a
next' (_ :< tail) = tail

