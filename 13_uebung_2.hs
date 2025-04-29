-- Abstract Syntax
-- ASTExpr represents the abstract syntax tree for expressions. It defines the structure of expressions in the language.
data ASTExpr
  = Var String  -- A variable, represented by its name (String).  Example: x
  | IntConst Integer  -- An integer constant, represented by its value (Integer). Example: 10
  | FloatConst Float  -- A floating-point constant, represented by its value (Float). Example: 3.14
  | Add ASTExpr ASTExpr  -- Addition of two expressions. The first ASTExpr is the left operand, the second is the right operand. Example: x + 5
  | Sub ASTExpr ASTExpr  -- Subtraction of two expressions.
  | Mul ASTExpr ASTExpr  -- Multiplication of two expressions.
  | Div ASTExpr ASTExpr  -- Division of two expressions.
  | Eq ASTExpr ASTExpr  -- Equality comparison of two expressions.  Example: x == y
  | Neq ASTExpr ASTExpr  -- Inequality comparison of two expressions. Example: x != y
  | Ge ASTExpr ASTExpr  -- Greater than or equal comparison. Example: x >= 0
  | Le ASTExpr ASTExpr  -- Less than or equal comparison. Example: x <= 10
  deriving (Show) -- Enables automatic generation of `show` function for debugging and printing ASTs.

-- ASTStatement represents the abstract syntax tree for statements. It defines the structure of statements in the language.
data ASTStatement
  = Skip  -- Represents a no-operation statement (does nothing).  Example: NOOP
  | Assign String ASTExpr  -- Represents an assignment statement.  The String is the variable name, and ASTExpr is the expression to assign. Example: x = 5
  | If ASTExpr ASTStatement ASTStatement  -- Represents an if-else statement. The ASTExpr is the condition, the first ASTStatement is the 'then' branch, and the second is the 'else' branch. Example: IF x > 0 THEN y = 1 ELSE y = 0
  | IfSimple ASTExpr ASTStatement  -- Represents a simple if statement (without an else branch). The ASTExpr is the condition, and the ASTStatement is the 'then' branch. Example: IF x > 0 THEN x = x - 1
  | While ASTExpr ASTStatement  -- Represents a while loop. The ASTExpr is the loop condition, and the ASTStatement is the loop body. Example: WHILE x < 10 DO x = x + 1
  | Seq [ASTStatement]  -- Represents a sequence of statements, contained in a list. Allows for multiple statements to be executed in order. Example: { x = 1; y = 2; z = x + y; }
  deriving (Show) -- Enables automatic generation of `show` function for debugging and printing ASTs.

-- ASTProgram represents the abstract syntax tree for an entire program.
data ASTProgram = Program String ASTStatement  -- Contains the program name (String) and the main statement (ASTStatement), which represents the program body. Example: PROGRAM MyProgram { x = 5; }
  deriving (Show) -- Enables automatic generation of `show` function for debugging and printing ASTs.

-- Parser
-- Parse2 is a newtype for creating parsers. It wraps a function that takes a String as input and returns a list of possible parses. Each parse is a tuple: (result, remainingInput).  This allows for backtracking and handling ambiguous grammars.
newtype Parse2 a = Parse (String -> [(a, String)])

-- Function to extract the underlying parsing function from a Parse2 value.
parse :: Parse2 a -> (String -> [(a, String)])
parse (Parse p) = p

-- Functor instance for Parse2.  Allows applying a function to the result of a parser.
instance Functor Parse2 where
  fmap f (Parse p) = Parse $ \s -> [(f a, s') | (a, s') <- p s] -- Applies the function 'f' to each parsed result 'a'.

-- Applicative instance for Parse2. Provides `pure` and `<*>`.
-- `pure`:  Creates a parser that always succeeds with the given value and doesn't consume any input.
-- `<*>`:  Applies a parser that produces a function to a parser that produces a value.
instance Applicative Parse2 where
  pure a = Parse $ \s -> [(a, s)] -- A parser that always succeeds with 'a' and leaves the input unchanged.
  (Parse pf) <*> (Parse px) = Parse $ \s -> [(f x, s'') | (f, s') <- pf s, (x, s'') <- parse (Parse px) s'] -- Applies the function produced by 'pf' to the value produced by 'px'.

-- Monad instance for Parse2. Provides `>>=` (bind). Enables sequencing parsers.
-- `>>=` (bind): Allows composing parsers sequentially.  The first parser's result is passed to a function that returns the next parser to execute.
instance Monad Parse2 where
  p >>= f = Parse $ \s -> concat [parse (f a) s' | (a, s') <- parse p s] -- Applies the function 'f' (which returns a parser) to each result 'a' of parser 'p'.

-- Parser combinators:  These are higher-order functions that combine existing parsers to create more complex ones.

-- mzero is the "zero" parser. It always fails, representing the absence of a valid parse.  Used in choice and to signal parse errors.
mzero :: Parse2 a
mzero = Parse $ \_ -> [] -- Returns an empty list, indicating failure.

-- (<|>), the choice combinator:  Allows a parser to try one parser, and if it fails, try another.  This implements "or" logic.
-- p <|> q attempts to parse with 'p'.  If 'p' fails, it attempts to parse with 'q'.  The result is the result of the first successful parse.
(<|>) :: Parse2 a -> Parse2 a -> Parse2 a
p <|> q = Parse $ \s -> case parse p s of
                          [] -> parse q s -- If 'p' fails (returns []), try 'q'.
                          x  -> x         -- Otherwise, return the result of 'p'.

-- many, the repetition combinator: Parses a parser zero or more times, collecting the results in a list.
-- many p parses 'p' repeatedly until 'p' fails. It returns a list of the results of 'p'. If 'p' never matches, it returns an empty list.
many :: Parse2 a -> Parse2 [a]
many p = many1 p <|> return [] -- Tries to parse 'p' one or more times (using many1), otherwise returns an empty list.

-- many1, the repetition combinator (one or more): Parses a parser one or more times, collecting the results in a list.
-- many1 p parses 'p' repeatedly until 'p' fails, but requires at least one successful parse.
many1 :: Parse2 a -> Parse2 [a]
many1 p = do x <- p; xs <- many p; return (x:xs) -- Parses 'p' once, then parses 'p' zero or more times.

-- optional, makes a parser optional:  Attempts to parse with the given parser.  If the parser succeeds, its result is returned in a `Just`. If it fails, it returns `Nothing`.
-- Useful for parsing optional elements in a grammar.
optional :: Parse2 a -> Parse2 (Maybe a)
optional p = (Just <$> p) <|> return Nothing -- Attempts to parse 'p'. If it succeeds, the result is wrapped in `Just`.  If it fails, returns `Nothing`.

-- try, the try combinator:  Wraps a parser and allows it to backtrack if it fails.  This is useful for handling ambiguous grammars where a parser might consume some input before failing.
-- Prevents a parser from consuming input if it fails, allowing other parsers to attempt to match.
try :: Parse2 a -> Parse2 a
try = id -- Simply returns the original parser. The behavior is implicitly handled by the `parse` function.

-- Basic Parsers:  These are the building blocks for creating more complex parsers.

-- item, parser for a single character: Consumes and returns a single character from the input string.
item :: Parse2 Char
item = Parse $ \s -> case s of
  []     -> []         -- If the input is empty, return failure (empty list).
  (c:cs) -> [(c, cs)]  -- Otherwise, return the first character 'c' and the rest of the string 'cs'.

-- sat, the satisfy combinator:  Takes a predicate (a function that takes a Char and returns a Bool) and applies it to the next character in the input.
-- If the predicate is true, the character is consumed and returned. If the predicate is false, the parser fails.
sat :: (Char -> Bool) -> Parse2 Char
sat p = do c <- item; if p c then return c else mzero -- Parses a character using 'item'. If the predicate 'p' is true for that character, it returns the character; otherwise, it fails.

-- expectChar, parser for a specific character: Expects a specific character and consumes it if found.
expectChar :: Char -> Parse2 Char
expectChar c = sat (== c) -- Uses `sat` with a predicate that checks for equality with the given character 'c'.

-- string, parser for a specific string:  Parses a specific string of characters.
string :: String -> Parse2 String
string [] = return [] -- If the string is empty, return an empty string (success).
string (c:cs) = do _ <- expectChar c; _ <- string cs; return (c:cs) -- Parses the first character, then recursively parses the rest of the string.

-- spaces, parser for spaces:  Skips a sequence of spaces (one or more).
spaces :: Parse2 ()
spaces = many (expectChar ' ') >> return () -- Parses spaces using `expectChar ' '` and `many` to handle multiple spaces.

-- Helper functions for character classification:  These functions are used to define predicates for the `sat` parser.
isLowerChar c = c >= 'a' && c <= 'z'  -- Checks if a character is a lowercase letter.
isUpperChar c = c >= 'A' && c <= 'Z'  -- Checks if a character is an uppercase letter.
isDigitChar c = c >= '0' && c <= '9'  -- Checks if a character is a digit.
isAlphaNumChar c = isLowerChar c || isUpperChar c || isDigitChar c -- Checks if a character is alphanumeric (letter or digit).

-- char, parser for a lowercase letter: Uses `sat` with `isLowerChar`.
char = sat isLowerChar

-- upper_char, parser for an uppercase letter: Uses `sat` with `isUpperChar`.
upper_char = sat isUpperChar

-- digit, parser for a digit: Uses `sat` with `isDigitChar`.
digit = sat isDigitChar

-- chardig_seq, parser for a sequence of letters and digits.
chardig_seq = many (sat isAlphaNumChar)  -- Parses zero or more alphanumeric characters.

-- digit_seq, parser for a sequence of digits.
digit_seq = many digit -- Parses zero or more digit characters.

-- identifier, parser for an identifier (variable name): Starts with a lowercase letter, followed by zero or more letters or digits.
identifier :: Parse2 String
identifier = do
  first <- char         -- Parses the first character (must be a lowercase letter).
  rest <- chardig_seq   -- Parses the rest of the identifier (zero or more alphanumeric characters).
  spaces                -- Consumes any trailing spaces.
  return (first : rest)   -- Returns the combined identifier string.

-- program_name, parser for a program name: Starts with an uppercase letter, followed by zero or more letters or digits.
program_name :: Parse2 String
program_name = do
  c <- upper_char       -- Parses the first character (must be an uppercase letter).
  rest <- chardig_seq   -- Parses the rest of the program name (zero or more alphanumeric characters).
  return (c : rest)       -- Returns the combined program name string.

-- integer, parser for an integer. Can be positive or negative.
integer :: Parse2 Integer
integer = do
  sign <- optional (expectChar '-')  -- Parses an optional minus sign.
  ds <- digit_seq                     -- Parses a sequence of digits.
  spaces                              -- Consumes any trailing spaces.
  if null ds then mzero else          -- If there are no digits, the parse fails.
    return $ case sign of
      Just _ -> -(read ds)           -- If there was a minus sign, negate the integer.
      Nothing -> read ds             -- Otherwise, read the integer as is.

-- float, parser for a floating-point number. Can be positive or negative.
float :: Parse2 Float
float = do
  sign <- optional (expectChar '-')  -- Parses an optional minus sign.
  intPart <- digit_seq                 -- Parses the integer part.
  _ <- expectChar '.'                -- Expects a decimal point.
  fracPart <- digit_seq                -- Parses the fractional part.
  spaces                               -- Consumes any trailing spaces.
  if null intPart || null fracPart then mzero else  -- Requires both integer and fractional parts.
    return $ case sign of
      Just _ -> -(read (intPart ++ "." ++ fracPart))  -- If there was a minus sign, negate the float.
      Nothing -> read (intPart ++ "." ++ fracPart)  -- Otherwise, read the float as is.

-- operator, parser for an arithmetic operator: +, -, *, or /.
operator :: Parse2 Char
operator = expectChar '+' <|> expectChar '-' <|> expectChar '*' <|> expectChar '/' -- Parses one of the arithmetic operators.

-- relator, parser for a relational operator: ==, /=, >=, or <=.
relator :: Parse2 String
relator = string "==" <|> string "/=" <|> string ">=" <|> string "<=" -- Parses one of the relational operators.

-- Expressions

-- expr, parser for expressions.  Uses choice to try different expression types.
expr :: Parse2 ASTExpr
expr = try parseOpExpr <|> parseNumber <|> parseVariable  -- Tries to parse an operator expression, a number, or a variable.

-- parseVariable, parser for a variable expression.
parseVariable = Var <$> identifier  -- Parses an identifier and creates a Var AST node.

-- parseNumber, parser for a number expression (integer or float).
parseNumber = try (FloatConst <$> float) <|> (IntConst <$> integer) -- Tries to parse a float or an integer.

-- parseOpExpr, parser for an expression with an operator (binary operation).
parseOpExpr = do
  op <- operator            -- Parses an operator.
  spaces                   -- Consumes spaces.
  e1 <- expr                -- Parses the first operand.
  spaces                   -- Consumes spaces.
  e2 <- expr                -- Parses the second operand.
  case op of
    '+' -> return (Add e1 e2)  -- Creates an Add AST node if the operator is '+'.
    '-' -> return (Sub e1 e2)  -- Creates a Sub AST node if the operator is '-'.
    '*' -> return (Mul e1 e2)  -- Creates a Mul AST node if the operator is '*'.
    '/' -> return (Div e1 e2)  -- Creates a Div AST node if the operator is '/'.
    _   -> mzero               -- Otherwise, the parse fails (shouldn't happen due to the operator parser).

-- pred_expr, parser for predicate expressions (boolean expressions).  These expressions evaluate to true or false.
pred_expr = do
  rel <- relator            -- Parses a relational operator.
  spaces                   -- Consumes spaces.
  e1 <- expr                -- Parses the first operand.
  spaces                   -- Consumes spaces.
  e2 <- expr                -- Parses the second operand.
  case rel of
    "==" -> return (Eq e1 e2)  -- Creates an Eq AST node if the operator is '=='.
    "/=" -> return (Neq e1 e2)  -- Creates a Neq AST node if the operator is '/='.
    ">=" -> return (Ge e1 e2)  -- Creates a Ge AST node if the operator is '>='.
    "<=" -> return (Le e1 e2)  -- Creates a Le AST node if the operator is '<='.
    _    -> mzero               -- Otherwise, the parse fails.

-- Statements

-- statement, parser for statements. Uses choice to parse different types of statements.
statement = parseNoop <|> parseAssign <|> parseIfElse <|> parseIf <|> parseWhile <|> parseBlock

-- parseNoop, parser for the NOOP (no operation) statement.
parseNoop = string "NOOP" >> spaces >> return Skip  -- Parses "NOOP" and returns a Skip AST node.

-- parseAssign, parser for assignment statements (variable assignment).
parseAssign = do
  var <- identifier       -- Parses the variable name.
  _ <- expectChar '='    -- Expects the '=' assignment operator.
  spaces                  -- Consumes spaces.
  e <- expr               -- Parses the expression to be assigned.
  return $ Assign var e  -- Creates an Assign AST node.

-- parseIfElse, parser for if-else statements.
parseIfElse = do
  string "IF"; spaces            -- Parses "IF".
  cond <- pred_expr; spaces     -- Parses the condition (a predicate expression).
  string "THEN"; spaces          -- Parses "THEN".
  s1 <- statement; spaces         -- Parses the 'then' branch statement.
  string "ELSE"; spaces          -- Parses "ELSE".
  s2 <- statement              -- Parses the 'else' branch statement.
  return $ If cond s1 s2        -- Creates an If AST node.

-- parseIf, parser for a simple if statement (without an else branch).
parseIf = do
  string "IF"; spaces            -- Parses "IF".
  cond <- pred_expr; spaces     -- Parses the condition.
  string "THEN"; spaces          -- Parses "THEN".
  s <- statement                -- Parses the 'then' branch statement.
  return $ IfSimple cond s      -- Creates an IfSimple AST node.

-- parseWhile, parser for while loops.
parseWhile = do
  string "WHILE"; spaces           -- Parses "WHILE".
  cond <- pred_expr; spaces      -- Parses the loop condition.
  string "DO"; spaces              -- Parses "DO".
  s <- statement                 -- Parses the loop body statement.
  return $ While cond s           -- Creates a While AST node.

-- parseBlock, parser for a block of statements (enclosed in curly braces).
parseBlock = do
  expectChar '{'; spaces          -- Expects an opening curly brace.
  stmts <- statement_seq          -- Parses a sequence of statements within the block.
  spaces; expectChar '}'          -- Expects a closing curly brace.
  return $ Seq stmts             -- Creates a Seq AST node containing the list of statements.

-- statement_seq, parser for a sequence of statements, separated by semicolons.
statement_seq :: Parse2 [ASTStatement]
statement_seq = do
  s <- statement                            -- Parses the first statement.
  rest <- many (expectChar ';' >> spaces >> statement)  -- Parses zero or more statements, each preceded by a semicolon.
  return (s : rest)                      -- Returns a list of statements.

-- program, parser for an entire program.
program :: Parse2 ASTProgram
program = do
  string "PROGRAM"; spaces              -- Parses "PROGRAM".
  name <- program_name; spaces           -- Parses the program name.
  stmt <- statement                      -- Parses the program body (a statement).
  expectChar '.'                       -- Expects a period at the end of the program.
  return $ Program name stmt             -- Creates a Program AST node.

-- parser2, alias for the main program parser.  This is the entry point for parsing a program.
parser2 :: Parse2 ASTProgram
parser2 = program

-- topLevel2, a helper function for running the parser. It simplifies the process of parsing a string.
topLevel2 :: Parse2 a -> String -> a
topLevel2 p s =
  case parse p s of
    [(result, _)] -> result  -- Returns the parsed result if the parsing is successful. Discards the remaining input.
    []            -> error "Parse unsuccessful: No parse found"  -- If no parse is found, it means the input string does not conform to the grammar.
    _             -> error "Parse unsuccessful: Ambiguous parse"  -- If more than one parse is found, it means the grammar is ambiguous.

-- Evaluation function to compute the value of an ASTExpr
evalExpr :: ASTExpr -> Float
evalExpr (Var _) = 0.0 -- Assuming variables are not defined, so default value 0.0
evalExpr (IntConst i) = fromIntegral i
evalExpr (FloatConst f) = f
evalExpr (Add e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Sub e1 e2) = evalExpr e1 - evalExpr e2
evalExpr (Mul e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (Div e1 e2) = evalExpr e1 / evalExpr e2

-- Example usage and tests
main :: IO ()
main = do
  let code = "PROGRAM Demo { x = + 1 2; y = * x 3; IF == x y THEN NOOP ELSE x = + x 1 }."
  let invalidCode = "PROGRAM Demo { x = + + 1 2;}."
  print $ topLevel2 parser2 code
  --print $ topLevel2 parser2 invalidCode

  let simpleExpr = Add (FloatConst 1.5) (FloatConst 2.5)
  let result = evalExpr simpleExpr
  
  putStrLn $ "Evaluating Add (FloatConst 1.5) (FloatConst 2.5): " ++ show result

  putStrLn $ "Testing basic parsers:"

  -- Tests for expectChar
  print $ topLevel2 (expectChar 'a') "a"

  -- Tests for string
  print $ topLevel2 (string "hello") "hello"

  -- Tests for upper_char
  print $ topLevel2 upper_char "A"

  -- Tests for digit
  print $ topLevel2 digit "1"

  -- Tests for chardig_seq
  print $ topLevel2 chardig_seq "abc123XYZ"
  print $ topLevel2 chardig_seq ""
  print $ topLevel2 chardig_seq "123abcXYZ"

  -- Tests for digit_seq
  print $ topLevel2 digit_seq "12345"
  print $ topLevel2 digit_seq ""

  -- Tests for identifier
  print $ topLevel2 identifier "var123"
  print $ topLevel2 identifier "var123abc"

  -- Tests for integer
  print $ topLevel2 integer "123"
  print $ topLevel2 integer "-123"

  -- Tests for float
  print $ topLevel2 float "123.456"
  print $ topLevel2 float "-123.456"

  -- Tests for operator
  print $ topLevel2 operator "+"

  -- Tests for relator
  print $ topLevel2 relator "=="

  -- Tests for expr
  print $ topLevel2 expr "- 1 2"
  print $ topLevel2 expr "- 5 2"
  print $ topLevel2 expr "* 3 4"
  print $ topLevel2 expr "/ 5 2.5"
  print $ topLevel2 expr "+ 1 -2"
  print $ topLevel2 expr "+ 1.5 2.5"
  print $ topLevel2 expr "+ 1 + 7 -2.5"

  -- Tests for pred_expr
  print $ topLevel2 pred_expr "/= 1 2"
  print $ topLevel2 pred_expr ">= 5 2"
  print $ topLevel2 pred_expr "<= 2 5"
  print $ topLevel2 pred_expr "== 1.0 1.0"
  print $ topLevel2 pred_expr "== + 1 2 3"
