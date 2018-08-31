module Main where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import System.Environment
import Data.Maybe
import qualified Data.Map as Map
import Tape
import Debug.Trace
import Control.Monad.State

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (runExpr expr)

runExpr :: String -> String
runExpr input = case parse sourceParser "sofun" input of
  Left err -> "No match: " ++ show err
  Right val -> show $ sfRun val Map.empty

-- data structures for parsing

data SofunToken = Number Double
                | Boolean Bool
                | BuiltIn Char
                | Identifier String
                | Stack SfStack
                deriving (Eq)

instance Show SofunToken where
  show (Number x) = (show x)++"\n"
  show (Identifier x) = "op "++x++"\n"
  show (Stack x) = "(\n"++(show x)++")\n"
  show (Boolean x) | x == True = "!\n"
                   | otherwise = "?\n"

data SfStack = SfStack [SofunToken]
                 deriving (Eq)

instance Show SfStack where
  show (SfStack x) = concatMap show x

instance Semigroup SfStack where
  (<>) (SfStack xs) (SfStack ys) = SfStack $ xs++ys

push :: SofunToken -> SfStack -> SfStack
push a (SfStack xs) = SfStack $ a:xs

pop :: SfStack -> SofunToken
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

data SfTail = SfTail SfStack SfStack -- condition and return stack

getCond (SfTail cond _) = cond
getBody (SfTail _ body) = body

data SfFun  = SfFun [SofunToken] [SfTail] -- args and tail

data SfSource = Fun (String, SfFun)
              | MainStack SfStack

-- functions for parsing

spaces :: Parser ()
spaces = skipMany1 space

specialCharacter :: Parser Char
specialCharacter = oneOf "!#$%&|*+-/:<=>?@^_~"

builtInParser :: Parser SofunToken
builtInParser = do symbol <- oneOf "+-*/<=^v#°"
                   spaces <|> eof
                   return $ BuiltIn symbol  

identifierParser :: Parser SofunToken
identifierParser = do first <- letter
                      rest <- many (letter <|> digit <|> specialCharacter)
                      spaces <|> eof
                      return $ Identifier (first:rest)

valueParser :: Parser SofunToken
valueParser = do dash <- option "" $ (string "-" <|> (char '+' >> return ""))
                 before <- many1 digit
                 dot <- option "" $ string "."
                 after <- option "" $ many digit
                 spaces <|> eof
                 return $ Number (read $ dash++before++dot++after)

booleanParser :: Parser SofunToken
booleanParser = do a <- oneOf "!?"
                   spaces <|> eof
                   return $ Boolean $ (\x -> if x=='!' then True else False) a
  
stackParser :: Parser SofunToken
stackParser = do char '('
                 spaces
                 (SfStack a) <- allSimpleTokenParser
                 many space
                 char ')'
                 spaces <|> eof
                 return $ Stack $ SfStack $ reverse $ a

allSimpleTokenParser :: Parser SfStack
allSimpleTokenParser = do
  a <- many (try valueParser <|> try booleanParser <|> try builtInParser <|>
             try identifierParser <|> try stackParser)
  return $ SfStack a

mainStackParser :: Parser SfStack
mainStackParser = do a <- allSimpleTokenParser
                     eof
                     return $ a

headParser :: Parser [SofunToken]
headParser = do a <- many1 identifierParser
                char ':'
                spaces
                return a

simpleTailParser :: Parser [SfTail]
simpleTailParser = do a <- allSimpleTokenParser
                      eof
                      return $ [SfTail (SfStack []) a] -- one stack without condition

branchParser :: Parser SfTail
branchParser = do a <- allSimpleTokenParser
                  char ':'
                  spaces
                  b <- allSimpleTokenParser
                  (oneOf "!?" >> spaces) <|> eof
                  return $ SfTail a b 

complexTailParser :: Parser [SfTail]
complexTailParser = do a <- many branchParser
                       return a

declParser :: Parser (String, SfFun)
declParser = do ((Identifier a):as) <- headParser
                b <- simpleTailParser <|> complexTailParser
                return $ (a, SfFun as b)

sourceParser :: Parser [SfSource]
sourceParser = do a <- many declParser
                  many space
                  string "\n"
                  many space
                  b <- mainStackParser
                  return $ (map Fun a)++[MainStack b]

-- data structures for evaluation

type SofunTape = Tape SofunToken

-- functions for evaluation

applyBuiltIn :: Char -> [SofunToken] -> ([SofunToken], [SofunToken])
-- arithmetics
applyBuiltIn '+' ((Number x):(Number y):xs)    = (xs,[Number $ x+y])
applyBuiltIn '-' ((Number x):(Number y):xs)    = (xs,[Number $ x-y])
applyBuiltIn '*' ((Number x):(Number y):xs)    = (xs,[Number $ x*y])
applyBuiltIn '/' ((Number x):(Number y):xs)    = (xs,[Number $ x/y])
-- logic
applyBuiltIn '<' ((Number x):(Number y):xs)    = (xs,[Boolean $ x<y])
applyBuiltIn '=' ((Number x):(Number y):xs)    = (xs,[Boolean $ x==y])
applyBuiltIn '=' ((Boolean x):(Boolean y):xs)  = (xs,[Boolean $ x==y])
-- stack
applyBuiltIn '^' (vx@(Number _):(Stack y):xs)  = (xs,[Stack $ push vx y])
applyBuiltIn 'v' ((Stack y):xs)                = (xs,[pop y])
applyBuiltIn '#' ((Stack y):xs)                = (xs,[Stack $ popped y])
applyBuiltIn '°' ((Stack y):xs)                = (xs,[Boolean $ isEmpty y])
applyBuiltIn a _ = error $ "built-in function applied to wrong arguments " ++ [a]

applyFun :: String -> [SofunToken] -> Map.Map String SfFun -> ([SofunToken], [SofunToken]) 
applyFun name xs funMap = helper fun
  where fun = if isJust f then fromJust f else error $ "function not found " ++ name
                  where f = Map.lookup name funMap
        helper fun@(SfFun fArgs _) = (drop (length fArgs) xs,
                                      replBody fArgs (findBody fun xs) xs)
        findBody (SfFun _ []) _ = error $ "tried to apply empty function " ++ name
        findBody (SfFun _ (fTail:[])) xs = getBody fTail
        findBody (SfFun fArgs (fTail:ts)) xs
          | isEmpty $ getCond fTail = getBody fTail
          | (pop $ sfEval (SfStack $ replBody fArgs (getCond fTail) xs) funMap) == Boolean True = getBody fTail
          | otherwise = findBody (SfFun fArgs ts) xs
        replBody fArgs (SfStack body) args = [fromMaybe x $ lookup x $ zip fArgs args | x <- body]

sfEval :: SfStack -> Map.Map String SfFun -> SfStack
sfEval (SfStack x) funMap = trace (show x) $ SfStack $ tapeToList $ helper (listToTape x) funMap
  where helper (Tape xs (BuiltIn x) ys) funMap = helper (Tape xsMinArgs retHead (retTail++ys)) funMap
          where xsMinArgs = fst $ applyBuiltIn x xs
                retStack  = snd $ applyBuiltIn x xs
                retHead   | (length retStack) > 0 = head retStack
                          | otherwise = error $ "applyBuiltIn returned nothing for " ++ [x]
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []
        helper (Tape xs (Identifier x) ys) funMap = helper (Tape xsMinArgs retHead (retTail++ys)) funMap
          where xsMinArgs = fst $ applyFun x xs $ funMap
                retStack  = snd $ applyFun x xs $ funMap
                retHead   | (length retStack) > 0 = head retStack
                          | otherwise = error $ "applyFun returned nothing for " ++ x
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []
        helper source@(Tape _ _ []) _ = source
        helper source@(Tape _ _ _) funMap = helper (moveRight source) funMap

sfRun :: [SfSource] -> Map.Map String SfFun -> SfStack
sfRun ((Fun (x,y)):xs) funMap = sfRun xs $ Map.insert x y funMap
sfRun ((MainStack x):[]) funMap = sfEval x funMap
sfRun _ _ = error $ "source code isn't in correct format"
