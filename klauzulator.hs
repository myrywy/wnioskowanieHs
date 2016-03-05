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
neg a = Negacja a

z1 = Elementarne "z1"
z2 = Elementarne "z2"
z3 = Elementarne "z3"
z4 = Elementarne "z4"
n1=neg ( neg (neg (neg z1)))

i12 = z1 ==> z2
r34 = z3 <=> z4
kr34 = odneguj( dem (klauzuluj r34) )

klauzuluj::Zdanie -> Zdanie
klauzuluj e@(Elementarne a) = e
klauzuluj neg@(Negacja (Negacja a) ) = klauzuluj a
klauzuluj neg@(Negacja (Elementarne a)) = neg
klauzuluj neg@(Negacja a) = Negacja (klauzuluj a)
klauzuluj (Implikacja a b) =  Alternatywa (Negacja (klauzuluj a)) (klauzuluj b)
klauzuluj (Rownowaznosc a b) = klauzuluj (Koniunkcja (Implikacja (klauzuluj a) (klauzuluj b)) (Implikacja (klauzuluj b) (klauzuluj a)))
klauzuluj (Koniunkcja a b) = klauzuluj (Negacja ( Alternatywa (Negacja (klauzuluj a)) (Negacja (klauzuluj b))) )
klauzuluj (Alternatywa a b) = Alternatywa (klauzuluj a) (klauzuluj b)

dem::Zdanie -> Zdanie
dem (Negacja (Alternatywa a b) ) = dem ( Koniunkcja (odneguj (Negacja (dem a))) (odneguj (Negacja (dem b))) )
dem (Negacja (Koniunkcja a b) ) = dem ( Alternatywa (odneguj (Negacja (dem a))) (odneguj (Negacja (dem b))) )
dem e@(Negacja a) = e
dem e@(Elementarne a) = e
dem (Alternatywa a b) = Alternatywa (odneguj (dem a)) (odneguj(dem b))
dem (Koniunkcja a b) = Koniunkcja (odneguj (dem a)) (odneguj(dem b))

odneguj (Negacja (Negacja a) ) = a
odneguj (Negacja a) = Negacja (odneguj a)
odneguj e@(Elementarne a) = e
odneguj (Alternatywa a b) = Alternatywa (odneguj a) (odneguj b)
odneguj (Koniunkcja a b) = Koniunkcja (odneguj a) (odneguj b)
odneguj (Implikacja a b) = Implikacja (odneguj a) (odneguj b)
odneguj (Rownowaznosc a b) = Rownowaznosc (odneguj a) (odneguj b)


pokazStr::Zdanie -> String
pokazStr (Rownowaznosc a b) = "( "++ pokazStr a ++"<=>"++ pokazStr b ++ " )"
pokazStr (Implikacja a b) = "( "++ pokazStr a ++"==>"++ pokazStr b ++ " )"
pokazStr (Alternatywa a b) = "( "++ pokazStr a ++" | "++ pokazStr b ++ " )"
pokazStr (Koniunkcja a b) = "( "++ pokazStr a ++" & "++ pokazStr b ++ " )"
pokazStr (Negacja a) = "( "++ "~"++pokazStr a ++ " )"
pokazStr (Elementarne str) = str

