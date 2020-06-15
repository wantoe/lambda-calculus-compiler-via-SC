--whet20

bmiTell :: ( RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You 're underweight , you emo , you !"
  | bmi <= normal = "You 're supposedly normal . Pffft , I bet you 're ugly !"
  | bmi <= fat = "You 're fat ! Lose some weight , fatty !"
  | otherwise = "You 're a whale , congratulations !"
    where 
      bmi = weight / height ^ 2
      (skinny , normal , fat) = (18.5 , 25.0 , 30.0)

tryfold :: [Int] -> Int
tryfold = foldr (+) 0 


treegun x = Node (Leaf) x (Node Leaf x Leaf)

length123 :: [Int] -> Int
length123 = foldr tick 0
  where tick _ x = x + 1




-- 2019 pp------------------------------------------------
data Tree a = 
    Leaf
  | Node (Tree a) a (Tree a)



fun1 Leaf = 0
fun1 (Node left x right) = fun1 left + x + fun1 right

fun2 Leaf = []
fun2 (Node left x right) = fun2 left ++ [x] ++ fun2 right 

--fun1 :: Tmd p -> p
tmap :: (Int -> Int) -> Tmd -> Tmd
tmap f (Varb x) = Varb (f x)
tmap f (Lamb x y) = Lamb (f x) (tmap f y)
tmap f (App n m) = App (tmap f n) (tmap f m) 

--tfold Leaf = 0
--tfold (Node left x right) = tfold left (+)   


{-
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f u [] = u
foldr f u (x:xs) = f x (foldr f u xs)

-}

fun1 = tfold (+) 0
  where  f l x r = l + x + r

tfold :: (Int -> Int -> Int -> Int) -> Int -> Tree -> Int
tfold f u Leaf = u
tfold f u (Node l a r) = f (tfold f u l) a (tfold f u r)


addone :: Int -> Int
addone = (+) 1

data Tmd =
    Varb Int  -- string
  | Lamb   Int  Tmd
  | App    Tmd Tmd

instance Show Tmd where
  show = pretty1

pretty1 :: Tmd -> String
pretty1 x = f 0 x
    where
      f i (Varb x) = show x
      f i (Lamb x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ show x ++ ". " ++ f 0 m 
      f i (App  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m


tmdexp :: Tmd
tmdexp = Lamb 1 (Lamb 2 (App (App (Lamb 3 (App (Varb 4) (Varb 5))) (Varb 6)) (Varb 7)))


-- 2019 pp ----------------------------------------------

-------------------------
-------- PART A --------- 
-------------------------

type Var = String

data Term =
    Variable Var  -- string
  | Lambda   Var  Term
  | Apply    Term Term
--  deriving Show

instance Show Term where
  show = pretty

example :: Term
example = Lambda "a" (Lambda "x" (Apply (Apply (Lambda "y" (Apply (Variable "a") (Variable "c"))) (Variable "x")) (Variable "b")))

example23 :: Term
example23 = Lambda "z" (Lambda "a" (Apply (Apply (Lambda "y" (Apply (Variable "z") (Variable "b"))) (Variable "a")) (Variable "c")))

eg :: Term
eg = Lambda "a" (Apply (Variable "b") (Variable "c"))


example22 = Apply (Lambda "a" (Variable "a"))(Variable "b")
example33 = Lambda "a" (Variable "a")
example44 = Apply (Variable "a")(Variable "b")
example55 = Variable "a"

pretty :: Term -> String
pretty = f 0
    where
      f i (Variable x) = x
      f i (Lambda x m) = if i /= 0 then "(" ++ s ++ ")" else s where s = "\\" ++ x ++ ". " ++ f 0 m 
      f i (Apply  n m) = if i == 2 then "(" ++ s ++ ")" else s where s = f 1 n ++ " " ++ f 2 m

------------------------- Assignment 1

numeral :: Int -> Term
numeral i = Lambda "f" (Lambda "x" (numeral' i))
  where
    numeral' i
      | i <= 0    = Variable "x"
      | otherwise = Apply (Variable "f") (numeral' (i-1))

-------------------------
-- merge and sort 2 lists, and also eliminate duplicate
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x == y    = x : merge xs ys
    | x <= y    = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

------------------------- Assignment 2
-- generate infinite variables
variables :: [Var]
variables = map (:[]) ['a'..'z'] ++ [ x : show i | i <- [1..] , x <- ['a'..'z'] ]

-- takes two lists of variables and returns 
--the first with all variables from the second list removed from it.
filterVariables :: [Var] -> [Var] -> [Var]
filterVariables xs []     = xs 
filterVariables xs (y:ys) = filter (/=y) (filterVariables xs ys)
-- tail recursion

filterVariables' :: [Var] -> [Var] -> [Var]
filterVariables' xs [] = xs
filterVariables' [] ys = []
filterVariables' (x:xs) (y:ys)
  | fn1 x (y:ys) = filterVariables' xs (y:ys)
  | otherwise    = x : (filterVariables' xs ys)
    where 
      fn1 x [] = False
      fn1 x (y:ys)
        | x == y    = True
        | otherwise = fn1 x ys

-- generate fresh variable
fresh :: [Var] -> Var
fresh = head . filterVariables variables

fresh' :: [Var] -> Var
fresh' [] = []
fresh' xs = (filterVariables variables xs) !! 0


-- collects all the variable names used in a Term, both 
-- as a Variable and in a Lambda abstraction
used :: Term -> [Var]
used (Variable x) = [x]
used (Lambda x n) = merge [x] (used n)
used (Apply  n m) = merge (used n) (used m)

------------------------- Assignment 3

rename :: Var -> Var -> Term -> Term
rename x y (Variable z)
  | x == z    = Variable y
  | otherwise = Variable z
rename x y (Lambda z n)
  | x == z    = (Lambda y (rename x y n))
  | otherwise = (Lambda z (rename x y n))
rename x y (Apply  n m) = (Apply (rename x y n) (rename x y m))

-- :(
substitute :: Var -> Term -> Term -> Term
substitute x n (Variable y)
    | x == y    = n
    | otherwise = Variable y
substitute x n (Lambda y m)
    | x == y    = Lambda y m
    | otherwise = Lambda z (substitute x n (rename y z m))
    where z = fresh (used n `merge` used m `merge` [x,y])
substitute x n (Apply m p) = Apply (substitute x n m) (substitute x n p)

-- substitute "b" (numeral 0) example
-- \d. \a. (\a. d c) a (\f. \x. x)
------------------------- Assignment 4

-- returns the list of all beta-reducts of a term
beta :: Term -> [Term]
beta (Apply (Lambda x n) m) =
  [substitute x m n] ++
  [Apply (Lambda x n') m  | n' <- beta n] ++
  [Apply (Lambda x n)  m' | m' <- beta m]
beta (Apply n m) =
  [Apply n' m  | n' <- beta n] ++
  [Apply n  m' | m' <- beta m]
beta (Lambda x n) = [Lambda x n' | n' <- beta n]
beta (Variable _) = []

-- (\a. \x. (\y. a c) x b) (\f. \x. f x)
-- reduces a term to normal form
normalize :: Term -> Term
normalize n
  | null ns   = n
  | otherwise = normalize (head ns) 
  where ns = beta n

normalize' :: Term -> Term
normalize' x 
  | null (beta x) = x
  | otherwise    = normalize' (head (beta x))

-- beta reduction and print all steps
run :: Term -> IO ()
run n = do
  print n
  let ns = beta n
  if null ns then
    return ()
  else
    run (head ns)

-------------------------

suc    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Variable "f") (Apply (Apply (Variable "n") (Variable "f")) (Variable "x")))))
add    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Variable "f")) (Apply (Apply (Variable "n") (Variable "f")) (Variable "x"))))))
mul    = Lambda "m" (Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Variable "m") (Apply (Variable "n") (Variable "f"))) (Variable "x")))))
dec    = Lambda "n" (Lambda "f" (Lambda "x" (Apply (Apply (Apply (Variable "n") (Lambda "g" (Lambda "h" (Apply (Variable "h") (Apply (Variable "g") (Variable "f")))))) (Lambda "u" (Variable "x"))) (Lambda "x" (Variable "x")))))
minus  = Lambda "n" (Lambda "m" (Apply (Apply (Variable "m") dec) (Variable "n")))

-------------------------
-------- PART B --------- 
-------------------------

------------------------- Assignment 5

-- returns the free variables of a term in an ordered list with no duplicates.
free :: Term -> [Var]
free (Variable x) = [x]
free (Lambda x y) = filter (/=x) (free y)
free (Apply n m) = merge (free n) (free m)


-- given a term N and a list of variables [x1,..., xn] returns the term Lambda x1,..., Lambda xn . N
abstractions :: Term -> [Var] -> Term
abstractions tm []     = tm
abstractions tm (x:xs) = Lambda x (abstractions tm xs)

-- given a term N and a list of variables which, given a term N and a list of terms [M1,..., Mn] returns the term N M1,...,Mn
applications :: Term -> [Term] -> Term
applications tm []     = tm
applications tm (x:xs) = applications (Apply tm x) xs

-- given a term N with free variables x1 through xn, 
-- returns the term (Lambda x1,..., Lambda xn . N) x1,..., xn
lift :: Term -> Term
lift x = applications (abstractions x (free x)) (varToTerm (free x))
  where
    varToTerm :: [Var] -> [Term]
    varToTerm []     = []
    varToTerm (x:xs) = (Variable x) : varToTerm xs

-- turns a term into a supercombinator expression 
super :: Term -> Term
super (Variable x)                        = Variable x
super (Lambda x y)                        = lift (exploreN (Lambda x y))
super (Apply n m)                         = Apply (super n) (super m)
-- AXUILLARY FUNCTION
exploreN :: Term -> Term
exploreN (Variable x) = Variable x
exploreN (Lambda x y) = Lambda x (exploreN y)
exploreN (Apply n m) = super (Apply n m)


-- variation of super
super' :: Term -> Term
super' x = lift (sup x) 
-- AXUILLARY FUNCTION
sup :: Term -> Term
sup (Variable x)                        = Variable x
sup (Lambda x y)                        = (Lambda x (sup y))
sup (Apply  (Lambda x y) (Lambda a b) ) = Apply (lift(Lambda x (sup y))) (lift(Lambda a (sup b)))
sup (Apply  (Lambda x y) zs )           = Apply (lift(Lambda x (sup y))) (sup zs)
sup (Apply  zs (Lambda x y) )           = Apply (sup zs) (lift(Lambda x (sup y)))
sup (Apply n m) = Apply (sup n) (sup m)


------------------------- Assignment 6
{-
type Var = String

data Term =
    Variable Var  -- string
  | Lambda   Var  Term
  | Apply    Term Term
  --  deriving Show

instance Show Term where
  show = pretty
-}

data Expr = 
    V Var
  | A Expr Expr

toTerm :: Expr -> Term
toTerm (V x) = Variable x
toTerm (A n m) = Apply (toTerm n) (toTerm m)

instance Show Expr where
  show = show . toTerm

-- Give a type Inst for instructions as a triple of a variable (the name), a list of variables
--(the parameters), and an expression. Un-comment the Show instance for Prog.

type Inst = (Var, [Var], Expr)

data Prog = Prog [Inst]


instance Show Prog where
  show (Prog ls) = unlines (map showInst ks)
    where
      ks = map showParts ls
      n  = maximum (map (length . fst) ks)
      showParts (x,xs,e) = (x ++ " " ++ unwords xs , show e)
      showInst (s,t) = take n (s ++ repeat ' ') ++ " = " ++ t


names = ['$':show i | i <- [1..] ]

-------------------------

-- strip the abstractions from a lambda-term: it should separate  \x1,...,\n . N  (where N is not an abstraction) into a pair ([x1; : : : ; xn];N) .
-- assumed input always starts with lambda term (\z.N) instead of (apply n m)
stripAbs :: Term -> ([Var],Term)
stripAbs x = (useFnA x, useFnB x) 
-- make up the [Var]
useFnA :: Term -> [Var]
useFnA (Variable x) = []
useFnA (Lambda x y) = funcA (Lambda x y)
useFnA (Apply n m) = []
--where 
funcA :: Term -> [Var]
funcA (Variable x)              = []
funcA (Lambda x (Lambda y zs) ) = [x] ++ funcA (Lambda y zs)
funcA (Lambda x (Apply n m) )   = [x]
funcA (Lambda x (Variable y))   = [x]
funcA (Apply n m)               = []

-- make up the Term section
useFnB :: Term -> Term
useFnB (Variable x) = Variable x
useFnB (Lambda x (Lambda y z)) = useFnB (Lambda y z)
useFnB (Lambda x y ) = funcB y
useFnB (Apply n m)  = Apply (funcB n) (funcB m)
--where
funcB :: Term -> Term
funcB (Variable x) = Variable x
funcB (Lambda x y) = (Lambda x y)
funcB (Apply n m)  = Apply n m
--funcB (Lambda x (Lambda y zs) ) = funcB (Lambda y zs)
--funcB (Lambda x (Apply n m) )   = funcB (Apply n m)
--funcB (Lambda x (Variable y))   = (Lambda x (Variable y))


-- to give the abstractions A in a lambda-term, following the grammar N ::= x | A | N N , as a list
takeAbs :: Term -> [Term]
takeAbs (Variable x) = []
takeAbs (Lambda x y) = [(Lambda x y)]
takeAbs (Apply n m)  = (takeAbs n) ++ (takeAbs m)


-- Term to Expression?
toExpr :: [Var] -> Term -> Expr
--toExpr names exmp = func01 names (useFnB exmp)
toExpr names exmp = func01 (take (length (takeAbs exmp)) names) (useFnB exmp)
--where
func01 :: [Var] -> Term -> Expr
func01 ss (Variable x)                      = V x
func01 (s:ss) (Lambda x y)                  = V (s)
func01 ss (Apply (Lambda x y) (Lambda a b)) = A (V (ss !! 0)) (V (ss !! 1))
func01 (s:ss) (Apply (Lambda x y) ms)       = A (V s) (func01 ss ms)
func01 ss (Apply ms (Lambda x y))           = A (func01 (take ((length ss)-1) ss) ms) (V (last ss))
--func01 ss (Apply n m)  = A (func01 (take (length (takeAbs n)) ss) n) (func01 ss m)
func01 ss (Apply n m)                       = A (func01 (take (length (takeAbs n)) ss) n) (func01 (drop (length (takeAbs n)) ss) m)

{- Backup codes
toExpr :: [Var] -> Term -> Expr
toExpr (s:ss) (Variable x) = V x
toExpr (s:ss) (Lambda x y) = V s
toExpr ss (Apply (Lambda x y) (Lambda a b)) = A (V (ss !! 0)) (V (ss !! 1))
toExpr (s:ss) (Apply (Lambda x y) ms)  = A (V s) (toExpr ss ms)
toExpr ss (Apply ms (Lambda x y))  = A (toExpr (ss) ms) (V (last ss))
toExpr ss (Apply n m)  = A (toExpr ss n) (toExpr ss m)
-}

-- uses 3 functions to output the triple tuple
toInst :: [Var] -> (Var,Term) -> (Inst,[(Var,Term)],[Var])
toInst nmes (var,tm)= (instST nmes (var,tm), createVarTermPair nmes tm,remainingFreshNames nmes tm)
--1st
instST :: [Var] -> (Var,Term) -> (Var, [Var], Expr)
instST nmes (main, example) = (main,useFnA example, (toExpr (take (length (createVarTermPair nmes example)) nmes) example))
--2nd
createVarTermPair :: [Var] -> Term -> [(Var,Term)]
createVarTermPair nmes x = zip nmes (abstractionOrApply x)
--where
abstractionOrApply :: Term -> [Term]
abstractionOrApply (Variable x) = []
abstractionOrApply (Lambda x (Lambda y z)) = abstractionOrApply (Lambda y z)
abstractionOrApply (Lambda x y) = extractLamBody y
abstractionOrApply (Apply n m) = (extractLamBody n) ++ (extractLamBody m)
--where
extractLamBody :: Term -> [Term]
extractLamBody (Variable x) = []
extractLamBody (Lambda x y ) = [Lambda x y]
extractLamBody (Apply n m) = (extractLamBody n) ++ (extractLamBody m)
--3rd
remainingFreshNames :: [Var] -> Term -> [Var]
remainingFreshNames nmes x = drop (length (createVarTermPair nmes x)) nmes


--prog example
prog :: Term -> Prog
prog exmp = Prog (aux names [("$main",super exmp)])

aux :: [Var] -> [(Var,Term)] -> [Inst]
aux (s:ss) []                  = []
aux (s:ss) ((x,Variable y):zs) = obtain_1st (toInst (s:ss) (x,Variable y)) : aux (remainingFreshNames (s:ss) (Variable y)) (zs ++ obtain_2nd (toInst (s:ss) (x,Variable y)))
aux (s:ss) ((x,Lambda y z):zs) = obtain_1st (toInst (s:ss) (x,Lambda y z)) : aux (remainingFreshNames (s:ss) (Lambda y z)) (zs ++ obtain_2nd (toInst (s:ss) (x,Lambda y z)))
aux (s:ss) ((x,Apply n m ):zs) = obtain_1st (toInst (s:ss) (x,Apply n m)) : aux (remainingFreshNames (s:ss) (Apply n m)) (zs ++ obtain_2nd (toInst (s:ss) (x,Apply n m)))

obtain_1st (a,_,_) = a
obtain_2nd (_,a,_) = a

-- The following examples are for testing codes
-- (\a. b) (\c. d)
example99 = Apply (Lambda "a" (Variable "b")) (Lambda "c" (Variable "d"))
-- (\a. b) (\c. d) (\x. y)
example100 = Apply (Apply (Lambda "a" (Variable "b")) (Lambda "c" (Variable "d"))) (Lambda "x" (Variable "y"))
example101 = Apply example100 (Lambda "g" (Variable "h"))
example102 = Apply example100 (Lambda "g" (Apply (Lambda "a" (Variable "b")) (Lambda "c" (Variable "d"))))

example2 = Apply (Variable "S") (Apply (Apply example (numeral 0)) (Variable "0"))
example3 = Apply (Apply add (numeral 1)) (Apply (Apply mul (numeral 2)) (numeral 3))
example4 = Apply (Apply example3 (Variable "S")) (Variable "0")

------------------------- Assignment 7
-- for reference
{-
Prog p = prog example2
Prog pee = prog example4
zs = [(x,e) | (x,_,e) <- p]
-}

sub :: [(Var,Expr)] -> Expr -> Expr
sub varexp (V ('$':s))   = matchDollorVar (V ('$':s)) (varexp)
sub varexp (V x)         = (V x)
sub varexp (A n m)       = A (sub varexp n) (sub varexp m) 
--where 
-- when $1 is found, loop through zs to find matching ones, then return expression needed for substitution
matchDollorVar :: Expr -> [(Var,Expr)] -> Expr
matchDollorVar (V ('$':s)) [] = V ('$':s)
matchDollorVar (V ('$':s)) (x:xs)
  | ('$':s) == fst x = snd x
  | otherwise        = matchDollorVar (V ('$':s)) xs
matchDollorVar (V s) _        = V s

-- 
step :: [Inst] -> [Expr] -> IO [Expr]
step progrm [] = return []
-- step progrm ((V "$main"):es) = return [(get_3rd (head progrm))]
step progrm ((V ('$':s)):es) = return (findDollar progrm ((V ('$':s)):es) )
step progrm ((V vr):[]) = do 
  putStrLn (vr++"\r")
  return [] 
step progrm ((V vr):es) = do 
  putStrLn (vr++"\r")
  return (es)
step progrm ((A n m):es) = return (n:m:es)
--WHERE
-- match the dollar sign from input to the ones in program, then choose the matched dollar sign item in the program
findDollar :: [Inst] -> [Expr] -> [Expr]
findDollar (p:ps) ((V ('$':s)) :es)
  | (get_1st p) == ('$':s) = do
    if (length (get_2nd p)) > (length es) then error "step: insufficient arguments on stack" -- raise error (as requested by pdf) when insufficient expressions on the stack to process all parameters
    else [abstractionA (get_3rd p) (formPair (get_2nd p) (take (length (get_2nd p)) es)) ] ++ (drop (length (get_2nd p)) es)-- apply es 
  | otherwise              = findDollar ps ((V ('$':s)) :es)
findDollar [] ((V ('$':s)) :es) = ((V ('$':s)) :es) -- when error occurs: 1st item of (it) cannot be found in program
--WHERE 
-- form pair of (lambda binding occurrence) and  input variables/expression for substitution
formPair :: [Var] -> [Expr] -> [(Var,Expr)]
formPair [] _ = []
formPair _ [] = []
formPair (x:xs) (y:ys) = (x,y) : (formPair xs ys)
-- formPair (get_2nd (p !! 1)) [(V "b"),(V "c"),(A (V "X") (V "Y")),(V "0")]
--WHERE
-- this is where substitution take place, match the 1st item of (lambda binding occurence and input variables/expression PAIR) with bound occurrences in Labmda body, then replace that with 2nd item of (lambda binding occurence and input variables/expression PAIR)
abstractionA :: Expr -> [(Var,Expr)] -> Expr
--abstractionA x (y:ys)
abstractionA (V vr) [] = V vr
abstractionA (V vr) (y:ys)
  | vr == (fst y) = (snd y)
  | otherwise   = abstractionA (V vr) ys
abstractionA (A n m) yss = A (abstractionA n yss) (abstractionA m yss)
-- for getting items in tuple
get_1st (a,_,_) = a
get_2nd (_,a,_) = a
get_3rd (_,_,a) = a


-- take a Term and print the normal form 
supernormalize :: Term -> IO ()
supernormalize tm = step' (transform (prog tm)) [V "$main"]
-- this function transform a Term to [Inst]
transform :: Prog -> [Inst]
transform (Prog instExmp) = instExmp
-- modified STEP function to print the normal form straight away by having recursion on the STEP function until no more dollar sign. This print / output lone Variables
step' :: [Inst] -> [Expr] -> IO ()
step' progrm [] = do
  return ()
step' progrm ((V ('$':s)):es) = do
  let answer = (findDollar progrm ((V ('$':s)):es) )
  step' progrm answer
step' progrm ((V vr):[]) = do 
  putStrLn (vr)
  return ()
step' progrm ((V vr):es) = do 
  putStrLn (vr++"\r")
  step' progrm es
step' progrm ((A n m):es) = do -- = return (n:m:es)
  step' progrm (n:m:es)