import Klauzulator
import Data.List

z1 = Elementarne "z1"
z2 = Elementarne "z2"
z3 = Elementarne "z3"
z4 = Elementarne "z4"

n1=neg z1
n2=neg z2
n3=neg z3
n4=neg z4

u = z1 ==> z2

-- a, b - klauzule
redukowalne::[Zdanie] -> [Zdanie] -> [Zdanie]
redukowalne [] [] = []
redukowalne [] _ = []
redukowalne _ [] = []
redukowalne xl@(x:xs) yl@(y:ys)
	| x `kompl` y = [absL x]
	| otherwise = nub((redukowalne xs yl) ++ (redukowalne xl ys))

usunPowt::(Ord a) => [a] -> [a]
usunPowt l = map head $ (group . sort) l

rowne::Zdanie -> Zdanie -> Bool
rowne a@(Elementarne _) b@(Elementarne _)
	| az == bz = True
	| otherwise = False
	where 
	(Elementarne az) = absL a
	(Elementarne bz) = absL b
rowne a@(Negacja _) b@(Elementarne _) = rowne b $ przeciwne a
rowne a@(Elementarne _) b@(Negacja _) = rowne a $ przeciwne b
rowne a@(Negacja _) b@(Negacja _) = rowne (przeciwne a) $ przeciwne b

komplementarne::Zdanie -> Zdanie -> Bool
komplementarne a b = a `rowne` (przeciwne b)
kompl = komplementarne

absL::Zdanie -> Zdanie
absL (Negacja a) = a
absL a = a

przeciwne:: Zdanie -> Zdanie
przeciwne (Negacja a) = a
przeciwne a = Negacja a


