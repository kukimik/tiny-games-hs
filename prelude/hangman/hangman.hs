#!/usr/bin/env runghc
{-# LANGUAGE CPP #-}
w = wordList !! ((`mod` length wordList) . read  . (filter (/= ':')) $ __TIME__)

--- cycling through the whole list by period s.t. length mod period = 1
-- or: after 5 words: "Enough playing, go back to work!"

chars = ' ':['a'..'z']
triples = sequence [chars,chars,chars]
wordList = words . concat $ map (\x-> triples !! ((fromEnum x) - 2^17)) "𣜈𡇄𠹇𠖱𤝈𠹬𡝡𠂔𠑔𣥻𢗻𣓙𠹀𢮞𢚦𢮍𠅮𢟽𠂷𢡃𢲝𠒙𢌢𠓾𣼭𠗛𠹐𠐽𠈂𢡥𡐗𠹉𠐽" -- 2^17 = 0x20000; U+20000 - początek długiego bloku CJK (U+20000 - U+2A6DF)

h = "  ___\n |  \\|\n o   |\n-+-  |\n/ \\  |\n   __|__\n"
m = "003330050042006000208790020:0;0020000112110" -- use order on chars to get 11 moves?
u k a b = if b <= toEnum (k + 48) then a else ' '

main = game 0 (replicate (length w) '.') -- use the interact "trick"

f x a '.' = if a == x then a else '.'
f _ _ b = b

compose :: [r -> r] -> r -> r
compose = flip (foldl (flip id))

nSucc n x = foldl (flip id) x (replicate n succ)

toUpper c = if elem c ['A'..'Z'] then compose (replicate 32 succ) c else c

i a b c = if a then b else c

game 11 _ = putStrLn "You lose!"
game n a = do
  putStrLn $ "\ESC[2J"++(zipWith (u n) h m)++a++"\n"
  if elem '.' a
    then
      do
        putStrLn $ "Choose a letter. Guesses: " ++ (show n) ++ "/11."
        ll <- getLine
        -- TODO: handle ll length (should be 1, and should be a char!), handle toUpper
        let l = head ll
        game (n + if elem l w then 0 else 1) (zipWith (f l) w a)
    else
      putStrLn $ a ++ "\nYou win!"

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
