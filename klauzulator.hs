data Zdanie = 
	Elementarne String 			-- Oznacznie zdania
	| Rownowaznosc Zdanie Zdanie
	| Implikacja Zdanie Zdanie
	| Koniunkcja Zdanie Zdanie
	| Alternatywa Zdanie Zdanie
	| Negacja Zdanie 
	deriving (Show)  

(==>) a b = Implikacja a b
(<=>) a b = Rownowaznosc a b
(&&&) a b = Koniunkcja a b
(|||) a b = Alternatywa a b
(~~~) a = Negacja a

z1 = Elementarne "z1"
z2 = Elementarne "z2"
z3 = Elementarne "z3"
z4 = Elementarne "z4"

i12 = z1 ==> z2
r34 = z3 <=> z4

klauzuluj::Zdanie -> Zdanie
klauzuluj e@(Elementarne a) = e
klauzuluj neg@(Negacja a) = neg
klauzuluj (Implikacja a b) =  Alternatywa (Negacja (klauzuluj a)) (klauzuluj b)
klauzuluj (Rownowaznosc a b) = klauzuluj (Koniunkcja (Implikacja (klauzuluj a) (klauzuluj b)) (Implikacja (klauzuluj b) (klauzuluj a)))
klauzuluj (Koniunkcja a b) = klauzuluj (Negacja ( Alternatywa (Negacja (klauzuluj a)) (Negacja (klauzuluj b))) )
klauzuluj (Alternatywa a b) = Alternatywa (klauzuluj a) (klauzuluj b)

pokazStr::Zdanie -> String
pokazStr (Rownowaznosc a b) = "( "++ pokazStr a ++"<=>"++ pokazStr b ++ " )"
pokazStr (Implikacja a b) = "( "++ pokazStr a ++"==>"++ pokazStr b ++ " )"
pokazStr (Alternatywa a b) = "( "++ pokazStr a ++" | "++ pokazStr b ++ " )"
pokazStr (Koniunkcja a b) = "( "++ pokazStr a ++" & "++ pokazStr b ++ " )"
pokazStr (Negacja a) = "( "++ "~"++pokazStr a ++ " )"
pokazStr (Elementarne str) = str

