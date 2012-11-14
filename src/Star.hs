module Star (

) where

import MParse
import Classes
import Instances
import System.IO (openFile, IOMode(ReadMode), hGetContents)
import Control.Monad (liftM)
import Prelude hiding (fmap, (>>=), (>>), fail, foldr, foldl)


-- ------------------
-- the token parsers


data Token =
  DataOpen String 
  | SaveOpen String
  | SaveClose
  | Whitespace String
  | Newline String
  | Comment String
  | Loop
  | Stop
  | Identifier String
  | Value String
  deriving (Show, Eq)
  
  
newline :: Parser Char Char
newline = mconcat (map literal "\n\r\f")


space :: Parser Char Char
space = satisfy (flip elem " \t\v")


blank :: Parser Char Char
blank = newline <|> space


comment :: Parser Char Token
comment = fmap Comment (literal '#' *> many (not1 newline))


dataOpen :: Parser Char Token
dataOpen = fmap DataOpen (string "data_" *> some (not1 blank))


saveOpen :: Parser Char Token
saveOpen = fmap SaveOpen (string "save_" *> some (not1 blank))


saveClose :: Parser Char Token
saveClose = string "save_" *> pure SaveClose


whitespace :: Parser Char Token
whitespace = fmap Whitespace (some space)


newlineT :: Parser Char Token
newlineT = fmap Newline (some newline)


stop :: Parser Char Token
stop = string "stop_" *> pure Stop


loop :: Parser Char Token
loop = string "loop_" *> pure Loop


identifier :: Parser Char Token
identifier = fmap Identifier (literal '_' *> some (not1 blank))


sqstring :: Parser Char String
sqstring = sq *> content <* sq
  where sq = literal '\''
        content = many (not1 sq <|> non_ending_sq)
         -- single quote followed by 'not a blank'
        non_ending_sq = sq <* switch blank

dqstring :: Parser Char String
dqstring = dq *> many (not1 dq) <* dq
  where dq = literal '"'


scstring :: Parser Char String
scstring = semic *> many (not1 endsequence) <* endsequence
  where
    semic = literal ';'
    endsequence = newline *> semic


special :: Parser Char Char
special = satisfy (flip elem "#_'\"")


uq :: Parser Char String
uq = fmap (:) (not1 $ blank <|> special) <*> many (not1 blank)


value :: Parser Char Token
value = fmap Value $ mconcat [sqstring, dqstring, scstring, uq]


oneToken :: Parser Char Token
oneToken = mconcat [dataOpen, saveOpen, saveClose, loop, stop, 
                    value, whitespace, newlineT, comment, identifier]


scanner :: Parser Char [Token]
scanner = many oneToken <* end


myReadFile :: String -> IO String
myReadFile path = 
  openFile path ReadMode >>= hGetContents


test = myReadFile "bmrb2.1.txt" >>= \str -> 
  return $ (getParser scanner) str


-------------
-- the rest of the parsers

-- looks like I'm just throwing away the names of the save frames
--   and the data blocks ... I wonder why ????
data AST
  = PLoop [Token] [Token]
  | PSave [AST]
  | PDatum Token Token
  | PData [AST]
  | PStar AST
  deriving (Show, Eq)


ident :: Parser Token Token
ident = satisfy isIdent
  where
    isIdent (Identifier x)   =  True
    isIdent   _              =  False


val :: Parser Token Token
val = satisfy isVal
  where
    isVal (Value v)   =  True
    isVal   _         =  False


saveme :: Parser Token Token
saveme = satisfy isSave
  where
    isSave (SaveOpen s)   =  True
    isSave   _            =  False


datame :: Parser Token Token
datame = satisfy isData
  where
    isData (DataOpen s)   =  True
    isData   _            =  False


pLoop :: Parser Token AST
pLoop = fmap PLoop (literal Loop *> many ident) <*> many val <* literal Stop

  
datum :: Parser Token AST
datum = fmap PDatum ident <*> val
  

pSave :: Parser Token AST
pSave = fmap PSave (saveme *> contents <* literal SaveClose)
  where 
    contents = some (datum <|> pLoop)


pData :: Parser Token AST
pData = fmap PData (datame *> some pSave)
  
  
pStar :: Parser Token AST
pStar = fmap PStar (pData <* end)


parseMe :: [Token] -> Maybe ([Token], AST)
parseMe = getParser pStar . filter notCommentOrWs
  where notCommentOrWs (Comment _) = False
        notCommentOrWs (Newline _) = False
        notCommentOrWs (Whitespace _) = False
        notCommentOrWs _ = True
  

testParse :: String -> Maybe ([Token], AST)
testParse = doTheParsing . getParser scanner
  where 
    doTheParsing (Just (_,x)) = parseMe x
    doTheParsing Nothing = Nothing
  
  
test2 = liftM testParse $ myReadFile "bmrb2.1.txt"


{-
test3 = liftM testParse $ myReadFile "bmrb3.0.txt"


hmm :: Show a => String -> Either (a, b) c -> String
hmm x (Right y) = x ++ "  Success!"
hmm x (Left z) = x ++ "  Failure ... " ++ (show $ fst z)


path = "../../../Desktop/nmr_software/new_bmrb_files/"
bigtest = getDirectoryContents path >>=
  (\x -> let them = filter (isPrefixOf "bmrb") x 
         in mapM (\y -> myReadFile (path ++ y) >>= 
                 (\z -> putStrLn $ hmm (y ++ " " ++ (show $ length z)) (testParse z))) them)
                 
                 
-- parseFile :: String -> IO (Either String ([Token], AST))
parseFile name = liftM testParse $ myReadFile (path ++ name)
  -}