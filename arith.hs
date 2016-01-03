import Text.ParserCombinators.Parsec
import Debug.Trace
import Control.Monad

--An interpreter of the arithmatic expressions as defined in Chapter 4 of Benjamin C. Pierce's "Types and Programming Lanuages".
--It's certainly not the best or most efficient implementation but it was mostly just for practice and learning both about Haskell and "Types and Programming Languages".
--A lot of parts are mostly copied from "Types and Programming Languages", "Learn You a Haskell", or "Real World Haskell" with small adjustments to make it all work.

data Term = TmTrue
	| TmFalse
	| TmIf Term Term Term
	| TmZero
	| TmSucc Term
	| TmPred Term
	| TmIsZero Term

--In theory, any well formed input expression will only cause this function to return "True", "False", "Zero", and/or some numeric value. However, I believe that if it is not a well formed expression then this will allow us to see where evaluation was at when evaluation stopped (unless it actually throws an error or something).
--I also only added the parenthesis to make the output look a little nicer.
output :: Term -> String
output TmTrue = "True"
output TmFalse = "False"
output (TmIf t1 t2 t3) = "(If " ++ output t1 ++ " then " ++ output t2 ++ " else " ++ output t3 ++ ")"
output TmZero = "Zero"
--I could possibly improve it later so that numeric values are translated into their natural number representations.
output (TmSucc t) = "(Succ " ++ output t ++ ")"
output (TmPred t) = "(Pred " ++ output t ++ ")"
output (TmIsZero t) = "(IsZero " ++ output t ++ ")"

isnumericval TmZero = True
isnumericval (TmSucc t1) = isnumericval t1
isnumericval _ = False

--I looked in the textbook and the given OCaml implementation of arith and it doesn't look like isval is actually used anywhere which is strange.
isval TmTrue = True
isval TmFalse = True
isval t
	| isnumericval t = True
isval _ = False

--Left means it's done evaluating, right means it still needs to be evaluated.
eval1 :: Term -> Either Term Term
eval1 (TmIf TmTrue t2 t3) = Right t2
eval1 (TmIf TmFalse t2 t3) = Right t3
eval1 (TmIf t1 t2 t3) = 
	let t1' = eval t1
	in Right (TmIf t1' t2 t3)
eval1 (TmSucc t1) = 
	let t1' = eval t1 
	in Left (TmSucc t1')
eval1 (TmPred TmZero) = Left TmZero
eval1 (TmPred (TmSucc nv1))
	| isnumericval nv1 = Left nv1
eval1 (TmPred t1) =
	let t1' = eval t1
	in Left (TmPred t1')
eval1 (TmIsZero TmZero) = Left TmTrue
eval1 (TmIsZero (TmSucc nv1))
	| isnumericval nv1 = Left TmFalse
eval1 (TmIsZero t1) =
	let t1' = eval t1
	in Left (TmIsZero t1')
eval1 x = Left x

eval :: Term -> Term
eval t = case (eval1 t) of
	Left x -> x
	Right x -> eval x

p_if :: GenParser Char st Term
p_if = do
	string "If"
	if_state <- p_term
	spaces
	string "Then"
	then_state <- p_term
	spaces
	string "Else"
	else_state <- p_term
	return (TmIf if_state then_state else_state)

p_term :: GenParser Char st Term
p_term = choice
	[(space >> p_term)
	, (string "True" >> return TmTrue)
	, (string "False" >> return TmFalse)
	, try p_if
	, (string "Zero" >> return TmZero)
	, (string "Succ" >> (fmap TmSucc p_term))
	, (string "Pred" >> (fmap TmPred p_term))
	, try (string "IsZero" >> (fmap TmIsZero p_term))]

handleError :: Either ParseError Term -> Term
handleError (Right worked) = worked
--This isn't a great solution but it's probably not worth spending the time on since this code isn't actually going to be used.
handleError _ = TmFalse

preParse :: GenParser Char st Term
preParse = do
	toReturn <- p_term
	eof
	return toReturn

parseTerm :: String -> Term
parseTerm input = handleError $ parse preParse "Parse Error" input

getAndEval :: String -> String
getAndEval = output . eval . parseTerm

main = forever $ do
	contents <- getLine
	putStrLn $ getAndEval contents
