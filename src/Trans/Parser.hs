{-# LANGUAGE NoMonomorphismRestriction
 #-}
module Trans.Parser (

) where

import Datums (Id(..))
import Classes
import Instances
import Trans.MTrans
import Trans.Instances
import Prelude hiding ((>>=), (>>), any, fmap)
import Combinators

-- requirements:
--   backtrack
--   parse tree has line/column
--   report errors
--   log whitespace and comments
--
-- monad stack (not necessarily in this order)
--   maybe (backtracking)
--   error (error reporting)
--   state (parse input)
--   state (line/column)
--   writer (whitespace/comments)
--   reader (track "stack trace" for error reports)
--
-- should be something like:  
--   s1 -> s2 -> r -> Either e (Maybe (w, (s1, (s2, a))))
--   variables:
--     s1: text input
--     s2: line/col
--     r: stack trace
--     e: stack trace
--     w: [Token]


type Parser s1 s2 r w e a = StateT s1 (StateT s2 (ReaderT r (WriterT w (ErrorT e Maybe)))) a

type Pos = (Int, Int)
type ErrorFrame = (String, Pos)
type Token = String

type P a = Parser String Pos [ErrorFrame] [Token] [ErrorFrame] a 

getPos = lift get
putPos = lift . put
updatePos f = getPos >>= (putPos . f)

bump :: Char -> P ()
bump '\n' = updatePos (\(l, c) -> (l + 1, 1))
bump '\r' = updatePos (\(l, c) -> (l + 1, 1))
bump '\f' = updatePos (\(l, c) -> (l + 1, 1))
bump _    = updatePos (\(l, c) -> (l, c + 1))

item :: P Char
item = get >>= f
  where
    f [] = none
    f (x:xs) = bump x *> put xs *> pure x

check :: (a -> Bool) -> P a -> P a
check pred p = 
    p >>= \v -> if (pred v) then (pure v) else none

satisfy :: (Char -> Bool) -> P Char
satisfy = flip check item

literal :: Char -> P Char
literal = satisfy . (==)

any :: AZero' f => [f a] -> f a
any = Prelude.foldr (<+>) zero

oneOf :: [Char] -> P Char
oneOf = any . map literal

not1 :: P a -> P Char
not1 p = switch p *> item

str :: [Char] -> P [Char]
str = commute . map literal

-- ----------------------

pushFrame message p =
    getPos >>= \pos ->
    local ((message, pos) :) p

err = 
    ask >>= throwE

commit p = p <+> err

-- why doesn't this compile?
-- cut = flip (pushFrame . commit)
cut :: String -> P a -> P a
cut message p = pushFrame message (commit p)

-- -------------------------

runParser :: P a -> String -> Maybe (Either [ErrorFrame] ([String], ((Int, Int), (String, a))))
runParser p xs = getErrorT (getWriterT (getReaderT (getStateT (getStateT p xs) (1, 1)) []))

run :: P a -> String -> ParseResult a
run p xs = unpack (runParser p xs)
  where
    unpack Nothing = Z
    unpack (Just (Left xs)) = E xs
    unpack (Just (Right (junk, (pos, (rest, value))))) = S (Success value rest pos junk)

data Success a
    = Success {value :: a, rest :: String, position :: (Int, Int), junkTokens :: [String]}
  deriving (Show)  

data ParseResult a
  = S (Success a)
  | E [ErrorFrame]
  | Z
  deriving (Show)

-- -------------------

{-
Grammar:

Whitespace  ::=  `/[ \t\n\r\f]+/`

Comment     ::=  `/;[^\n\r\f]*/`

Number      ::=  `/\d+/`

Symbol      ::=  `/\w+/`

List        ::=  '['  Form(*)  ']'

App         ::=  '('  Form  Form(*)  ')'

Def         ::=  '{'  define  Symbol  Form  '}'

Lambda      ::=  '{'  lambda  '['  Symbol(*)  ']'  Form  '}'

Cond        ::=  '{'  cond  '{'  ('{'  Form  Form  '}')(*)  '}'  Form  '}'

Form        ::=  Number  |  Symbol  |  List  |  App  |  Lambda  |  Cond  |  Def

File        ::=  Form(*)

-}

whitespace :: P String
whitespace = many1 (oneOf " \t\n\r\f")

comment :: P String
comment = pure (:) <*> literal ';' <*> many0 (not1 (oneOf "\n\r\f"))

junk = (whitespace <+> comment) >>= \x -> write [x]

number :: P String
number = many1 (oneOf "0123456789")

symbol :: P String
symbol = many1 (satisfy (\c -> c <= 'z' && c >= 'a'))

tok p = p <* many0 junk

list_ = 
    tok (literal '[')   *> 
    many0 form         <* 
    (cut "]" $ tok (literal ']'))

list :: P [Form]
list = pushFrame "list" list_ 

app_ = 
    pure (\a b c d -> (b, c)) <*>
    tok (literal '(')         <*>
    pushFrame "operator" form <*>
    many0 form                <*>
    pushFrame ")" (tok $ literal ')') 

app :: P (Form, [Form])
app = pushFrame "application" app_

data Form
  = Num  String
  | Sym  String
  | List [Form]
  | App Form [Form]
  | Lambda [String] Form
  | Cond [(Form, Form)] Form
  | Def String Form
  deriving (Show)
  
form = 
    fmap Num  (tok number) <+> 
    fmap Sym  (tok symbol) <+> 
    fmap List list         <+>
    special                <+>
    fmap (uncurry App) app

oc = tok (literal '{')
cc = tok (literal '}')

def :: P Form
def = pushFrame "define" (pure (\_ -> Def)          <*> 
                          tok (str "define")        <*> 
                          cut "symbol" (tok symbol) <*> 
                          cut "form" form)

lambda :: P Form
lambda = pushFrame "lambda" (pure (\_ _ s _ f -> Lambda s f) <*>
                             tok (str "lambda")              <*>
                             oc                              <*>
                             many0 (tok symbol)              <*>
                             cut "}" cc                      <*>
                             cut "form" form)

condBranch = pushFrame "cond branch" (pure (\_ a b _ -> (a, b)) <*> 
                                      oc                        <*> 
                                      cut "predicate" form      <*> 
                                      cut "result" form         <*> 
                                      cut "}" cc)

cond :: P Form
cond = pushFrame "cond" (pure (\_ _ cs _ f -> Cond cs f) <*>
                         tok (str "cond") <*>
                         oc               <*>
                         many0 condBranch <*>
                         cut "}" cc       <*>
                         cut "else" form)

special :: P Form
special = pushFrame "special form" (tok (literal '{') *> cut "body" (def <+> lambda <+> cond) <* cut "}" (tok $ literal '}'))

file :: P [Form]
file = many0 junk *> many0 form

