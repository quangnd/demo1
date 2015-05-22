doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
						then x
						else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1			
boomBangs xs = [if (x < 10) then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase :: [Char] -> [Char] --declare type
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

--factorial :: Integer -> Integer
--factorial n = product [1..n]

--Functions
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "one!";
sayMe 2 = "two!";
sayMe 3 = "three!";
sayMe x = "Not between 1 and 5";

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on empty list, dummy"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two element: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty strings, whoops!"
capital all@(x:xs) = "The first  letter of " ++ all ++ " is " ++ [x]

--GUARDS--
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
	| weight / height ^ 2 <= 25.0 = "You're supposedly normal, Pffft, I bet you're ugly!" 
	| weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty"
	| otherwise = "You're whale, congraluations"

--bmiTell better with WHERE
--
bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
	| bmi <= skinny = "You're underweight, you emo, you!"
	| bmi <= normal = "You're supposedly normal, Pffft, I bet you're ugly!" 
	| bmi <= fat    = "You're fat! Lose some weight, fatty"
	| otherwise     = "You're whale, congraluations"
	where 
		bmi = weight / height ^ 2
		(skinny, normal, fat) = (18.5, 25.0, 30.0)
	  
calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
	where bmi weight height = weight / height ^ 2

max' :: (Ord a) => a -> a -> a
max' a b 
	| a > b = a
	| otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
	| a > b       = GT
	| a == b 	  = EQ
	| otherwise   = LT


initials :: String -> String -> String
initials firstname lastname = [f] ++ [d] ++ ". " ++ [l] ++ "."
	where
		(f:d:_) = firstname
		(l:_) = lastname