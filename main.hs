module Main where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Data.Maybe
import qualified Data.Map as Map
import Tape
import Debug.Trace
import Control.DeepSeq

main :: IO ()
main = do 
         expr <- getContents
         expr `deepseq` (putStrLn $
                         runExpr $
                         filter (\x -> x/="" && (head x) /= '#') $
                         lines expr)
         

runExpr :: [String] -> String
runExpr sfLines = show $ sfRun (map helper $ zip [1..] sfLines) Map.empty 
  where helper (line, input) = case parse sourceParser ("(Line "++(show line)) input of
          Left err -> error $ "error while parsing: " ++ (show err)
          Right val -> val

-- data structures for parsing

data SfToken = Number Double
                | Boolean Bool
                | BuiltIn Char
                | Identifier String
                | Stack SfStack
                deriving (Eq)

instance Show SfToken where
  show (Number x) = (show x)++" "
  show (Identifier x) = x++" "
  show (Stack (SfStack x)) = "( "++(concatMap show $ reverse x)++") "
  show (Boolean x) | x == True = "§ "
                   | otherwise = "$ "
  show (BuiltIn x) = x:" "

data SfStack = SfStack [SfToken]
                 deriving (Eq)

instance Show SfStack where
  show (SfStack x) = concatMap show x

instance Semigroup SfStack where
  (<>) (SfStack xs) (SfStack ys) = SfStack $ xs++ys

push :: SfToken -> SfStack -> SfStack
push a (SfStack xs) = SfStack $ a:xs

pop :: SfStack -> SfToken
pop (SfStack []) = error "tried to pop an empty stack"
pop (SfStack xs) = head xs

popped :: SfStack -> SfStack
popped (SfStack []) = error "tried to popped an empty stack"
popped (SfStack xs) = SfStack $ tail xs

isEmpty :: SfStack -> Bool
isEmpty (SfStack []) = True
isEmpty _ = False

instance Monoid SfStack where
  mempty = SfStack []
  mappend (SfStack xs) (SfStack ys) = SfStack $ xs++ys

data SfTail = SfTail SfStack SfStack deriving (Show) -- condition and return stack

getCond (SfTail cond _) = cond
getBody (SfTail _ body) = body

data SfFun  = SfFun [SfToken] [SfTail] deriving (Show) -- args and tail

data SfSource = Fun (SfToken, SfFun) -- name and fun
              | MainStack SfStack deriving (Show)

-- functions for parsing

spaces :: Parser ()
spaces = skipMany1 space

comment :: Parser ()
comment = do char '#'
             return ()

specialCharacter :: Parser Char
specialCharacter = (oneOf "!>@_{[]}´`'\"\\.,~&|ł€ŧ←↓→øþſæđŋħĸł»«¢„“”µ·…") <?> "special character"

reservedCharacter = (oneOf "+-*/<=^v°$§():;?") <?> "reserved Character"

builtInParser :: Parser SfToken
builtInParser = do symbol <- oneOf "+-*/<;=^%v°" <?> "built in"
                   spaces <|> eof <|> comment
                   return $ BuiltIn symbol  

reservedButLonger :: Parser Char
reservedButLonger = do a <- reservedCharacter
                       lookAhead $ many1 (alphaNum <|> reservedCharacter)
                       return $ a

identifierParser :: Parser SfToken
identifierParser = do first <- (letter <|> reservedButLonger <|> specialCharacter)
                      rest <- many (letter <|> digit <|> specialCharacter <|> reservedCharacter)
                      spaces <|> eof <|> comment
                      return $ Identifier (first:rest)

valueParser :: Parser SfToken
valueParser = do dash <- option "" $ (string "-" <|> (char '+' >> return ""))
                 before <- many1 digit
                 dot <- option "" $ string "."
                 after <- option "" $ many digit
                 spaces <|> eof <|> comment
                 return $ Number (read $ dash++before++dot++after)

booleanParser :: Parser SfToken
booleanParser = do a <- oneOf "§$"
                   spaces <|> eof <|> comment
                   return $ Boolean $ (\x -> if x=='§' then True else False) a
  
stackParser :: Parser SfToken
stackParser = do char '('
                 spaces
                 (SfStack a) <- allSimpleTokenParser
                 many space
                 char ')'
                 spaces <|> eof <|> comment
                 return $ Stack $ SfStack $ reverse a

allSimpleTokenParser :: Parser SfStack
allSimpleTokenParser = do
  a <- many (try valueParser <|> try booleanParser <|> try builtInParser <|>
             try identifierParser <|> try stackParser)
  return $ SfStack a

mainStackParser :: Parser SfSource
mainStackParser = do a <- allSimpleTokenParser
                     eof <|> comment
                     return $ MainStack $ a

headParser :: Parser [SfToken]
headParser = do a <- many1 $ try identifierParser
                skipMany space
                char ':' <|> char '?'
                spaces
                return $ a

simpleTailParser :: Parser [SfTail]
simpleTailParser = do a <- allSimpleTokenParser
                      eof <|> comment
                      return $ [SfTail (SfStack []) a] -- one stack without condition

branchParser :: Parser SfTail
branchParser = do a <- allSimpleTokenParser
                  char ':'
                  spaces
                  b <- allSimpleTokenParser
                  (char '?' >> spaces)
                  return $ SfTail a b

complexTailParser :: Parser [SfTail]
complexTailParser = do a <- many1 $ try branchParser
                       b <- simpleTailParser
                       return $ a++b

declParser :: Parser SfSource
declParser = do as <- headParser
                b <- try simpleTailParser <|> try complexTailParser
                return $ Fun $ ((last as), SfFun (init as) b)
                
sourceParser :: Parser SfSource
sourceParser = do a <- declParser <|> mainStackParser
                  eof <|> comment
                  return $ a

-- functions for evaluation

applyBuiltIn :: Char -> [SfToken] -> ([SfToken], [SfToken])
-- arithmetics
applyBuiltIn '+' ((Number x):(Number y):xs)    = (xs,[Number $ y+x])
applyBuiltIn '-' ((Number x):(Number y):xs)    = (xs,[Number $ y-x])
applyBuiltIn '*' ((Number x):(Number y):xs)    = (xs,[Number $ y*x])
applyBuiltIn '/' ((Number x):(Number y):xs)    = (xs,[Number $ y/x])
applyBuiltIn '%' ((Number x):(Number y):xs)    = (xs,[Number $
                                                      fromIntegral $
                                                      mod (floor $ x) (floor $ y)])
-- logic
applyBuiltIn '<' ((Number x):(Number y):xs)    = (xs,[Boolean $ y<x])
applyBuiltIn '=' ((Number x):(Number y):xs)    = (xs,[Boolean $ x==y])
applyBuiltIn '=' ((Boolean x):(Boolean y):xs)  = (xs,[Boolean $ x==y])
-- stack
applyBuiltIn '^' (vx@(Number _):(Stack y):xs)  = (xs,[Stack $ push vx y])
applyBuiltIn 'v' ((Stack y):xs)                = (xs,[pop y])
applyBuiltIn ';' ((Stack y):xs)                = (xs,[Stack $ popped y])
applyBuiltIn '°' ((Stack y):xs)                = (xs,[Boolean $ isEmpty y])
applyBuiltIn a _ = error $ "built-in function applied to wrong arguments " ++ [a]

applyFun :: String -> [SfToken] -> Map.Map String SfFun -> ([SfToken], [SfToken]) 
applyFun name xs funMap = helper fun
  where fun = if isJust f then fromJust f else error $ "function not found " ++ name
                  where f = Map.lookup name funMap
        helper fun@(SfFun fArgs _) = (drop (length fArgs) xs,
                                      replBody fArgs (findBody fun xs) xs)
        findBody (SfFun _ []) _ = error $ "tried to apply empty function " ++ name
        findBody (SfFun _ (fTail:[])) xs = getBody fTail
        findBody (SfFun fArgs (fTail:ts)) xs
          | isEmpty $ getCond fTail = getBody fTail
          | condReturn == Boolean True = getBody fTail
          | condReturn == Boolean False =  findBody (SfFun fArgs ts) xs
          | otherwise = error $ "condition didn't return boolean " ++ (show $ getCond fTail) ++ " " ++ (show $ condReturn)
          where condReturn = (pop $ sfEval (SfStack $ replBody fArgs (getCond fTail) xs) funMap)
        replBody fArgs (SfStack body) args | length args >= length fArgs = traceShowId $ [lookAndExchange x $ zip (reverse fArgs) args | x <- body]
                                           | otherwise = error $ "function applied to too few arguments " ++ name
          where lookAndExchange (Stack x) table = Stack $ SfStack $ replBody fArgs x args
                lookAndExchange x table = fromMaybe x $ lookup x table

sfEval :: SfStack -> Map.Map String SfFun -> SfStack
sfEval (SfStack x) funMap = SfStack $ tapeToList $ helper (listToTape x) funMap
  where helper source@(Tape xs (BuiltIn x) ys) funMap
          | isJust retHead = traceShow source $ helper (Tape xsMinArgs (fromJust retHead) (retTail++ys)) funMap
          | ys /= [] = traceShow source $ helper (Tape xsMinArgs (head ys) (tail ys)) funMap
          | otherwise = traceShow source $ (Tape xsMinArgs (Identifier "") [])
          where xsMinArgs = fst $ applyBuiltIn x xs
                retStack  = snd $ applyBuiltIn x xs
                retHead   | (length retStack) > 0 = Just $ head retStack
                          | otherwise = Nothing
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []
        helper source@(Tape xs (Identifier x) ys) funMap
          | isJust retHead = traceShow source $ helper (Tape xsMinArgs (fromJust retHead) (retTail++ys)) funMap
          | ys /= [] = traceShow source $ helper (Tape xsMinArgs (head ys) (tail ys)) funMap
          | otherwise = traceShow source $ (Tape xsMinArgs (Identifier "") [])
          where xsMinArgs = fst $ applyFun x xs $ funMap
                retStack  = snd $ applyFun x xs $ funMap
                retHead   | (length retStack) > 0 = Just $ head retStack
                          | otherwise = Nothing
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []
        helper source@(Tape _ _ []) _ = traceShow source $ source
        helper source@(Tape _ _ _) funMap = traceShow source $ helper (moveRight source) funMap

sfRun :: [SfSource] -> Map.Map String SfFun -> SfStack
sfRun ((Fun ((Identifier x),y)):xs) funMap = traceShow x $ sfRun xs $ Map.insert x y funMap
sfRun ((MainStack x):[]) funMap = sfEval x funMap
sfRun x _ = error $ "source code isn't in correct format" ++ (show x)
