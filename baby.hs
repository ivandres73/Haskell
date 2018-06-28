doubleUs x y = doubleMe x + doubleMe y

doubleMe x = x + x

doubleSmallNumber x = if x > 100
						then x
						else x*2

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!"
				| x <- xs, odd x]

length' xs = sum [ 1 | _ <- xs]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st =
	[c | c <- st, c `elem` ['A'..'Z'] || c == ' ']

correct :: String -> String
correct str =
	[
	  	if c == 'S' then '5'
	  	else if c == 'O' then '0'
	  	else if c == 'I' then '1'
	  	else c
	  	| c <- str
  	]

mySplit num list = 
	[
		take num list,
		drop num list
	]

myIncmin :: [Int] -> [Int]
myIncmin lista = 
	[ l+1 | l <- lista]

factorial :: Int -> Int
factorial x = if x > 1
			then x * factorial (x-1)
			else 1