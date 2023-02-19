import Data.List(elemIndex)
import Data.Maybe(fromJust)

chars = ' ':['a'..'z']
triples = sequence [chars,chars,chars]
wordList = "shuffle jazzy weather emanate microbe cohomology monad functor applicative bane same sane game lame"
tri (a:b:c:l) = [a,b,c]:(tri l)
tri [] = []

encoded = (encode <$> tri wordList)

encode :: String -> Char
encode = (\t -> toEnum . (+ 0x20000) . fromJust $ elemIndex t triples)
main = putStrLn (encode <$> tri wordList) >> putStrLn (concat $ (map (\x-> triples !! ((fromEnum x) - 0x20000)) "𣜈𡇄𠹇𠖱𤝈𠹬𡝡𠂔𠑔𣥻𢗻𣓙𠹀𢮞𢚦𢮍𠅮𢟽𠂷𢡃𢲝𠒙𢌢𠓾𣼭𠗛𠹐𠐽𠈂𢡥𡐗𠹉𠐽"))


