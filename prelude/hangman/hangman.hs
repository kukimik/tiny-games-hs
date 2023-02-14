#!/usr/bin/env runghc
{-# LANGUAGE CPP #-}
wordList = words "shuffle jazzy weather emanate microbe cohomology monad functor applicative bane same sane game lame"
w = wordList !! ((`mod` length wordList) . read  . (filter (/= ':')) $ __TIME__)

--- cycling through the whole list by period s.t. length mod period = 1
-- or: after 5 words: "Enough playing, go back to work!"

h = "  ___\n |  \\|\n o   |\n-+-  |\n/ \\  |\n   __|__\n"
m = (read . (\x->[x])) <$> "0033300500420060002087800209090020000112110" -- use order on chars to get 11 moves?
u k a b = if b <= k then a else ' '

main = game 0 (replicate (length w) '.') -- use the interact "trick"

f x a '.' = if a == x then a else '.'
f _ _ b = b

compose :: [r -> r] -> r -> r
compose = flip (foldl (flip id))

toUpper c = if elem c ['A'..'Z'] then compose (replicate 32 succ) c else c


game 9 _ = putStrLn "You lose!"
game n a = do
  putStrLn $ "\ESC[2J"++(zipWith (u n) h m)++a++"\n"
  if elem '.' a
    then
      do
        putStrLn "Choose a letter. Guesses: " ++ show n ++ "/11."
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
8780020
9090020
00011211 
-}
