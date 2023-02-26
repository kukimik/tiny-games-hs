#!/usr/bin/env runghc
{-# LANGUAGE CPP #-}{-# LANGUAGE MultiWayIf #-}
w = wordList !! ((`mod` length wordList) . read $ [x|x<-__TIME__,x/=':'])
decode=(>>=(\x->triples!!((fromEnum x)-2^17)))
--- cycling through the whole list by period s.t. length mod period = 1
-- or: after 5 words: "Enough playing, go back to work!"

chars = ' ':['a'..'z']
triples = mapM id=<<[[chars,chars,chars],[chars,chars],[chars]] -- why only chars here? maybe use larger set and compress the hangman picture?

wordList = words . decode $ "𣜈𡇄𠹇𠖱𤝈𠹬𡝡𠂔𠑔𣥻𢗻𣓙𠹀𢮞𢚦𢮍𠅮𢟽𠂷𢡃𢲝𠒙𢌢𠓾𣼭𠗛𠹐𠐽𠈂𢡥𡐗𠹉𠐽" -- 2^17 = 0x20000; U+20000 - początek długiego bloku CJK (U+20000 - U+2A6DF)

h = "  ___\n |  \\|\n o   |\n-+-  |\n/ \\  |\n   __|__\n"
m = "aadddaafaaecaagaaacaihjaacakalaacaaaabbcbba" -- use characters and compress!

u k a b|b <= toEnum (k + 97)=a|1>0=' '

main = game 0 ('.'<$w)

f x a '.'|a==x=a|1>0='.'
f _ _ b=b

toLower c = if elem c ['A'..'Z'] then shift 32 c else c

i a b c = if a then b else c

shift n = toEnum . (+ n) . fromEnum

game n a = do
  putStrLn $ "\ESC[2J"++(zipWith (u n) h m)++'\n':a++"\n\n"
  if | n == 11 -> putStrLn "You lose!"
     | elem '.' a -> 
      do
        putStrLn $ "Choose a letter. Wrong guesses: " ++ (show n) ++ "/11."
        ls <- ((fmap toLower)<$>) $ getLine
        case ls of l:[] -> game (n + (fromEnum $ elem l ['a'..'z']) - (fromEnum $ elem l w)) (zipWith (f l) w a); _ -> game n a
     | 1>0 -> putStrLn "You win!"

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
