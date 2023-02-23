#!/usr/bin/env runghc
{-# LANGUAGE CPP #-}{-# LANGUAGE MultiWayIf #-}
w = wordList !! ((`mod` length wordList) . read  . (filter (/= ':')) $ __TIME__)

--- cycling through the whole list by period s.t. length mod period = 1
-- or: after 5 words: "Enough playing, go back to work!"

chars = ' ':['a'..'z']
triples = sequence [chars,chars,chars] -- why only chars here? maybe use larger set and compress the hangman picture?
wordList = words . concat $ map (\x-> triples !! ((fromEnum x) - 2^17)) "𣜈𡇄𠹇𠖱𤝈𠹬𡝡𠂔𠑔𣥻𢗻𣓙𠹀𢮞𢚦𢮍𠅮𢟽𠂷𢡃𢲝𠒙𢌢𠓾𣼭𠗛𠹐𠐽𠈂𢡥𡐗𠹉𠐽" -- 2^17 = 0x20000; U+20000 - początek długiego bloku CJK (U+20000 - U+2A6DF)

h = "  ___\n |  \\|\n o   |\n-+-  |\n/ \\  |\n   __|__\n"
m = "003330050042006000208790020:0;0020000112110" -- use characters and compress!
u k a b = if b <= toEnum (k + 48) then a else ' '

main = game 0 ('.'<$w) -- use the interact "trick"  interact $ go 0 (replicate (length w) '.') w 

f x a '.' = if a == x then a else '.'
f _ _ b = b

toLower c = if elem c ['A'..'Z'] then shift 32 c else c

i a b c = if a then b else c

shift n = toEnum . (+ n) . fromEnum

go n a w (c:cs) = 
  "\ESC[2J"++(zipWith (u n) h m)++a++"\n"
  ++ if | n==11 -> "You lose!" 
        | elem '.' a -> "Choose a letter. Wrong guesses: " ++ (show n) ++ "/11." ++ (go (n + if elem c w then 0 else 1) (zipWith (f c) w a) w cs)
        | otherwise -> "\nYou win!"

game n a = do
  putStrLn $ "\ESC[2J"++(zipWith (u n) h m)++'\n':a++"\n"
  if | n == 11 -> putStrLn "\nYou lose!"
     | elem '.' a -> 
      do
        putStrLn $ "Choose a letter. Wrong guesses: " ++ (show n) ++ "/11."
        l:m <- ((fmap toLower)<$>) $ getLine
        game (n + fromEnum (m\=[]&&elem l ['a'..'z']&&elem l w)) (zipWith (f l) w a)
     | 1>0 -> putStrLn "\nYou win!"

{-
  ___\n
 |  \|\n
 o   |\n
-+-  |\n
/ \  |\n
   __|__

003330
0500420
0600020
8790020
:0;0020
00011211 
-}
