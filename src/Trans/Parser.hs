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

{-
Grammar:

Whitespace  ::=  `/[ \t\n\r\f]+/`

Comment     ::=  `/;[^\n\r\f]*/`

Number      ::=  `/\d+/`

Symbol      ::=  `/\w+/`

List        ::=  '('  Form(*)  ')'

Form        ::=  Number  |  Symbol  |  List

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

pushFrame message p =
    getPos >>= \pos ->
    local ((message, pos) :) p

err = 
    ask >>= throwE

commit p = p <+> err

list_ = 
    tok (literal '(')   *> 
    many0 form         <* 
    pushFrame ")" (commit $ tok (literal ')'))

list :: P [Form]
list = pushFrame "list" list_ 

data Form
  = Num  String
  | Sym  String
  | List [Form]
  deriving (Show)
  
form = 
    fmap Num  (tok number) <+> 
    fmap Sym  (tok symbol) <+> 
    fmap List list

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

