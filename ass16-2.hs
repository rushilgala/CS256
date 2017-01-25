{-	CS256 Functional Programming
 -	Rushil Gala-Shah (u1515140)
 -	Coursework Part 2
 -	============================
 -}
{-	Parse tree generator for Shaggy
 -	===============================
 -}
{-
 -  Shaggy constant
 -  ===============
 -	Shaggy constant consists of an unsigned decimal numeral
 -}
{- Convert char value to Int -}
val :: Char -> Int
val '0' = 0
val '1' = 1
val '2' = 2
val '3' = 3
val '4' = 4
val '5' = 5
val '6' = 6
val '7' = 7
val '8' = 8
val '9' = 9
val c   = error (c:" is not decimal digit") -- Error checking
{- Convert a string of decimal digits into an integer using higher order functions. -}
convertString xs = sum terms  														-- sum the numbers
          where
            pows = 1 : [ 10*i | i <- pows ]								-- powers
            digits [] = []												-- empty list
            digits (x:ys) = (val x) : (digits ys)						-- convert element into int
            terms = [ i*j | (i,j) <- (zip (reverse(digits xs)) pows) ]	-- place in terms multiply digit by power to get right value
{-
 -	Determine whether a string represents a constant by checking each value is is between 0 and 9 inclusive.
 -	Testing this:
 -	iscon "120"
 -	True
 -	iscon "s120"
 -	False
 -	iscon "1s20"
 -	False
 -	iscon "12s0"
 -	False
 -	iscon "120s"
 -	False
 -	iscon ""
 -	False
 -	iscon "test"
 -	False
 -}
iscon :: String -> Bool
iscon [] = False								            -- empty then return false
iscon (x:l)
	| (x<='9' && x>='0') && l == []	= True	  -- If its been true and that was the last digit, then return true
	| (x<='9' && x>='0') 			      = iscon l -- Check next digit in the list
	| otherwise						          = False 	-- Otherwise return false
{-
 -  Shaggy variable
 -  ===============
 -	Shaggy variable is a finite alphanumeric string beginning with a letter
 -  -----------------------------------------------------------------------
 -	Determine whether a string represents a variable by checking if the first character is a letter.
 -	Testing this:
 -	isvar "123"
 -	False
 -	isvar "a123"
 -	True
 -	isvar "1a23"
 -	False
 -	isvar ""
 -	False
 -}
isvar :: String -> Bool
isvar [] = False 						-- If empty, return false
isvar (x:l) = ('a' <= x) && (x <= 'z')	-- If the first character is a letter, return true, otherwise return false
{-
 -  Shaggy Program
 -  ==============
 -	Shaggy program is a finite sequence (list) of definitions
 -}
type Program = [Definition]
{-
 -  Show function for a Shaggy program.
 -  ===================================
 -  There is a new line character that prints a new line for each definition when using putStr
 -}
showprog :: Program -> String
showprog [] = []
showprog (s:p) = ((showdef s) ++ "\n") ++ (showprog p)
{-
 -  Get Program
 -  ===========
 -  Get the program as a string and put in list for each line.
 -  Uses the function getWords, get, and remove
 -  getWords takes a string as an argument and get/remove take a string and char as an argument
 -  Returns a list containing a list of strings, which are simply the original string split by \n character
 -}
getProg :: String -> [[String]]
getProg [] = []
getProg prog = [getWords (get prog '\n')] ++ getProg(remove prog '\n')
{-
 -  Get Words
 -  =========
 -  getWords function returns a list of Strings
 -  The words are from each line of strings initiated by getProg
 -  Words are separated as spaces, so the get requests splits the string by spaces
 -  Returns a list of strings, split by the space character
 -}
getWords :: String -> [String]
getWords [] = []
getWords w = [get w ' '] ++ getWords (remove w ' ')
{-
 -  Get function
 -  ============
 -  The get function takes a string and character as its arguments and outputs a String
 -  An empty string will return an empty String
 -  Otherwise, if the character in the string matches the input character, then return
 -  If theres a bracket, and the character input is a space, then launch the bracket parser
 -  Otherwise recursively, go through the rest of the string
 -}
get :: String -> Char -> String
get [] a = []
get (x:xs) a
          | x == a = []
          | a == ' ' && x == '(' = x : doBracket 1 xs
          | otherwise = x : get xs a
{-
 -  Remove function
 -  ===============
 -  Takes a string and character as an argument
 -  If we have an empty string, and the character is a space, then empty string
 -  If theres an open bracket, then process the expression in the process
 -  If the current character matches the input character (space or newline), then get the rest of the String
 -  Otherwise, recursively look at the rest of the string
 -}
remove :: String -> Char -> String
remove [] a = []
remove (r:rs) a
              | rs == [] && a == ' ' = []
              | r == '(' && a == ' ' = finishBracket 1 rs
              | r == a = rs
              | otherwise = remove rs a
{-
 -  Do Bracket
 -  ==========
 -  Takes an integer and string as its arguments
 -  If the count is 0, return
 -  If an opening parenthesis is encountered, then add one to the count,
 -  If a closing parenthesis is encountered, then subtract one from the count
 -  Otherwise keep processing the bracket until the count is 0
 -  The initial count (when using this function after encountering a bracket) is 1
 -  The additional counts are to check if further opening brackets are encountered
 -}
doBracket :: Int -> String -> String
doBracket 0 s = []
doBracket a (b:br)
                | b == '(' = b : doBracket (a+1) br
                | b == ')' = b : doBracket (a-1) br
                | otherwise = b : doBracket a br
{-
 -  Finish Bracket
 -  ==============
 -  Takes an integer and string as its arguments
 -  If the count is 0 and the string is empty, then return an empty String
 -  If the count is 0 and there is a space, then get the rest of the String
 -  If the count is 0, then keep recursively going until the string is empty
 -  If an opening parenthesis is encountered, add one to the count
 -  If a closing parenthesis is encountered, then subtract one from the count
 -  Oherwise keep processing the string until the count is 0 and the string is empty
 -  The count is used to make sure we have an even number of parenthesis
 -  Initially, the count is 1
 -}
finishBracket :: Int -> String -> String
finishBracket 0 [] = []
finishBracket 0 (b:br)
                | b == ' ' = br
                | otherwise = (b:br)
finishBracket a (b:br)
                | b == '(' = finishBracket (a+1) br
                | b == ')' = finishBracket (a-1) br
                | otherwise = finishBracket a br
{-
 -  Parser
 -  ======
 -  The parser turns an input string into a Shaggy Program
 -  getProg will have a list of list of strings that is the input for parse function
 -  The parse function takes a list of list of strings and turns it into a shaggy program
 -  This covers:
 -  Declaring variables
 -  Assigning an expression to a variable
 -  Assigning two expressions and an operator to a variable
 -  Negative expression being assigned to a variable
 -  Evaluating an expression which is either a constant or variable
 -}
parser :: String -> Program
parser s = parse(getProg s)

parse :: [[String]] -> Program
parse []							              = []
parse ( ["var", v] : p )			      = (Var (Va v)) : (parse p)						               -- Declare variable
parse ( [v, "=", e] : p )			      = (Assign (Va v) (expList [e])) : (parse p)			     -- Assign a expression to a variable
parse ( [v, "=", op, e1, e2] : p )	= (Assign (Va v) (expList [op, e1, e2])) : (parse p) -- Assign an operation (Add,Minus,Multiply) to a variable
parse ( [v, "=", "-", e] : p )		  = (Assign (Va v) (expList ["-", e])) : (parse p)		 -- Minus expression also has a - const/- exp
parse ( ["eval", e] : p )			      = (Eval (isexp e)) : (parse p)					             -- Eval evalutes an expression which is either a constant or variable
{-
 -  Expression list
 -  ===============
 -  expList takes a list of strings and turns it into an expression
 -  The expression will be used in the parser
 -  This covers all two argument expressions with operations, negative expressions, brackets, constants and variables
 -}
expList :: [String] -> Exp
expList [] = error ("No expression")
expList ["+", e1, e2] = Op Add (expList [e1]) (expList [e2])         -- Addition
expList ["-", e1, e2] = Op Min (expList [e1]) (expList [e2])         -- Subtraction
expList ["*", e1, e2] = Op Mul (expList [e1]) (expList [e2])         -- Multiplication
expList ["-", e]      = Minus (expList [e])                          -- Negative number
expList [('(':xs)]    = Bracket (expList (getWords (deBracket xs)))  -- Brackets
expList [e]
  | iscon e   = Const (convertString e)                              -- Constant case.
  | isvar e   = Va e                                                 -- Variable case.

{-
 -  Debracket
 -  =========
 -  Returns the same string without the last element of the String
 -  The primary use is to remove the trailing parenthesis ')' from an expression with brackets
 -}
deBracket :: String -> String
deBracket s = take ((length s) - 1) s
{-
 -	Shaggy definition
 -  =================
 -	Variable declaration var
 -	var, assignment and evaluation/return value.
 -}
data Definition = Var Exp 	-- Variable declaration var : var
 					| Assign Exp Exp 	-- Assignment
 					| Eval Exp 			  -- Evaluate expression where expression is a constant/variable
instance Show Definition where
  show = showdef
{-
 -  Show function for definitions.
 -  ==============================
 -  Showdef outputs Definition as a String
 -  Var is a declaration of var as var
 -  Assign is when an expression is assigned to a variable
 -  Eval is when an expression is evaluated, and expression is either a constant/variable
 -}
showdef :: Definition -> String
showdef (Var (Va v))     	= (showvar (Va v))
showdef (Assign (Va v) e) = (showassign (Va v)) ++ " =" ++ (showexp e)
showdef (Eval e)			    = "eval" ++ (showexp e)
{-
 -  Operations
 -  ==========
 -  Operations allowed in a Shaggy expression
 -}
data Oper = Add | Mul | Min deriving (Show, Eq)
{-
 -  Show function for operation symbols.
 -  ====================================
 -  Used in showexp to output operators as a string
 -}
showop :: Oper -> String
showop Add = " +"             -- Addition symbol.
showop Mul = " *"             -- Multiplication symbol.
showop Min = " -"             -- Subtraction.
{-
 -	Shaggy Expressions.
 -  ===================
 -	Expressions contain constants, variables, operations, Bracket and Minus
 -	The Minus Exp allows - constant or - expression
 -  The constant is an integer
 -  The variable is a String
 -  The Bracket contains an expression
 -  The operation contains the operator and two expressions in its arguments
 -}
data Exp = Const Int | Va String | Minus Exp | Op Oper Exp Exp | Bracket Exp deriving (Show)
{- Check to see if we have 0 arguments. -}
isexp :: String -> Exp
isexp e
  | iscon e   = Const (convertString e)   -- Constant case.
  | isvar e   = Va e           -- Variable case.
{-
 -  Show variables as var "name"
 -  =============================
 -  Takes an expression (Declaration) and outputs it as a String
 -  Simply adds var before the variable
 -}
showvar :: Exp -> String
showvar (Va e) = "var " ++ e
{-
 -  Show assignment as variable name
 -  ================================
 -  Takes an expression and outputs the variable name
 -  Used in show definition
 -}
showassign :: Exp -> String
showassign (Va e) = e
{-
 -  Show function for expressions.
 -  ===============================
 -  Takes an expression and turns it into a String
 -  Can either have a constant, Variable, Operation, Minus, or Bracket expression
 -  Change this to change the output format
 -}
showexp :: Exp -> String
showexp (Const e) = " " ++ (show e)                                     -- Constant case.
showexp (Va e)  = " " ++ e                                           	  -- Variable case.
showexp (Op op e1 e2) = (showop op) ++ (showexp e1) ++ (showexp e2)   	-- Two argument expressions.
showexp (Minus e) =	" -" ++ (showexp e)					            -- Negative expression/constant
showexp (Bracket e) =  " (" ++ (showexp e) ++ ")"                        -- Bracketed expression

{-
 -	Testing
 -  =======
 -  Output for test1:
 -  ------------------
 -  putStr test1
 -  var x
 -  x = - 42
 -  var y
 -  y = + x 1
 -  eval y
 -  Output for test2
 -  ------------------
 -  putStr test2
 -  var x
 -  x = 4
 -  var y
 -  y = - 3 x
 -  x = + ( - y 1) 2
 -  eval x
 -}
t1 :: String
t1 = "var x\nx = - 42\nvar y\ny = + x 1\neval y"
test1 = showprog (parser t1)
t2 :: String
t2 = "var x\nx = 4\nvar y\ny = - 3 x\nx = + (- y 1) 2\neval x"
test2 = showprog (parser t2)
