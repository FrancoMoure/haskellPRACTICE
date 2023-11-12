--this is gonna be written in spanish beacuse the university that I am attending is sittuated in Argentina.
--With this, I wanna prove that I have a basic knowledge of functional programming, even though I wanna get a job as a Junior Frontend Developer. Also, I can prove with this that I have mathematical skills and that I am fluent in spanish (which could be obvious beacuse I am from Argentina).
--every exercise name is gonna be translated into english so you can understand everything.

--1 (sucesor means "the next number")
sucesor :: Int -> Int 
sucesor x = x + 1

--2 (sumar means "add")
sumar :: Int -> Int -> Int
sumar x y = x + y

--3 (division y resto means "division and remainer")
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto a b = (a `div` b, a `mod` b)

--4 (max del par means "maximum of the pair")
maxDelPar :: (Int,Int) -> Int
maxDelPar (x,y) = if x > y 
                    then x 
                    else y

--5
