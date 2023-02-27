import Data.List(elemIndex)
import Data.Maybe(fromJust)
import Data.Char(isPrint)

chars = ['a'..'z']++" /?.:YWEPCB"
triples = mapM id=<<[[chars,chars,chars]]

tri :: String -> [String]
tri (a:b:c:l) = [a,b,c]:(tri l)
tri [] = []

encodeTriple :: Int -> String -> Char
encodeTriple n = (\t -> toEnum . (+ n) . fromJust $ elemIndex t triples)

encode :: Int -> String -> String 
encode n x = (encodeTriple n <$> tri x)

strings =
  [
   "shuffle jazzy weather emanate microbe cohomology monad functor applicative bane same sane game lame"
  ,"You win. "
  ,"You lose."
  ,"aadddaafaaecaagaaacaihjaacakalaacaaaabbcbbaaa"
  ,"Wrong guesses: "
  ,"Enough playing. Back to work. "
  ,"Play again? y/n"
  ,"Enter a letter."
  ]

x n = (encode n) <$> strings

try n = 
  case (filter (not . isPrint) $ concat (x n)) of
    [] -> putStrLn . unlines $ (x n)
    u -> do
          putStrLn $ (show n)++":"++(show $ ((triples !!).(+ (-n)).fromEnum) <$> u)
          try (n+1)

main = try 256

  


