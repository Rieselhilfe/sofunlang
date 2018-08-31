module Main where
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import System.Environment
import Data.Maybe
import Tape
import Debug.Trace

main :: IO ()
main = do 
         (expr:_) <- getArgs
         putStrLn (runExpr expr)

runExpr :: String -> String
runExpr input = case parse mainStackParser "sofun" input of
  Left err -> "No match: " ++ show err
  Right val -> show $ sfEval $ val

--data structures for parsing

data SofunToken = Number Double
                | Boolean Bool
                | Operator String
                | Stack SofunSource
                deriving (Eq)

instance Show SofunToken where
  show (Number x) = (show x)++"\n"
  show (Operator x) = "op "++x++"\n"
  show (Stack x) = "(\n"++(show x)++")\n"
  show (Boolean x) | x == True = "!\n"
                   | otherwise = "?\n"

data SofunSource = SofunSource [SofunToken]
                 deriving (Eq)

instance Show SofunSource where
  show (SofunSource x) = concatMap show x

instance Semigroup SofunSource where
  (<>) (SofunSource xs) (SofunSource ys) = SofunSource $ xs++ys

push :: SofunToken -> SofunSource -> SofunSource
push a (SofunSource xs) = SofunSource $ a:xs

pop :: SofunSource -> SofunToken
pop (SofunSource []) = error "tried to pop an empty stack"
pop (SofunSource xs) = head xs

popped :: SofunSource -> SofunSource
popped (SofunSource []) = error "tried to popped an empty stack"
popped (SofunSource xs) = SofunSource $ tail xs

isEmpty :: SofunSource -> Bool
isEmpty (SofunSource []) = True
isEmpty _ = False

instance Monoid SofunSource where
  mempty = SofunSource []
  mappend (SofunSource xs) (SofunSource ys) = SofunSource $ xs++ys

--functions for parsing

spaces :: Parser ()
spaces = skipMany1 space

specialCharacter :: Parser Char
specialCharacter = oneOf "!#$%&|*+-/:<=>?@^_~"

operatorParser :: Parser SofunToken
operatorParser = do first <- letter <|> specialCharacter
                    rest <- many (letter <|> digit <|> specialCharacter)
                    spaces <|> eof
                    return $ Operator (first:rest)

valueParser :: Parser SofunToken
valueParser = do dash <- option "" $ string "-"
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
                 a <- many (try valueParser <|> try operatorParser <|> try stackParser)
                 many space
                 char ')'
                 spaces <|> eof
                 return $ Stack $ SofunSource $ reverse $ a

mainStackParser :: Parser SofunSource
mainStackParser = do a <- many (try valueParser <|> try operatorParser <|> try stackParser)
                     eof
                     return $ SofunSource a

--data structures for evaluation

type SofunTape = Tape SofunToken

--functions for evaluation

applyOp :: String -> [SofunToken] -> ([SofunToken], [SofunToken])
--arithmetics
applyOp "+" ((Number x):(Number y):xs)    = (xs,[Number $ x+y])
applyOp "-" ((Number x):(Number y):xs)    = (xs,[Number $ x-y])
applyOp "*" ((Number x):(Number y):xs)    = (xs,[Number $ x*y])
applyOp "/" ((Number x):(Number y):xs)    = (xs,[Number $ x/y])
--logic
applyOp "<" ((Number x):(Number y):xs)    = (xs,[Boolean $ x<y])
applyOp "=" ((Number x):(Number y):xs)    = (xs,[Boolean $ x==y])
applyOp "=" ((Boolean x):(Boolean y):xs)  = (xs,[Boolean $ x==y])
--stack
applyOp "^" (vx@(Number _):(Stack y):xs)  = (xs,[Stack $ push vx y])
applyOp "v" ((Stack y):xs)                = (xs,[pop y])
applyOp "#" ((Stack y):xs)                = (xs,[Stack $ popped y])
applyOp "°" ((Stack y):xs)                = (xs,[Boolean $ isEmpty y])
applyOp a _ = error $ "couldn't apply function "++a

sfEval :: SofunSource -> SofunSource
sfEval (SofunSource x) = SofunSource $ tapeToList $ helper $ listToTape x
  where helper (Tape xs (Operator x) ys) = helper (Tape xsMinArgs retHead (retTail++ys))
          where xsMinArgs = fst $ applyOp x xs
                retStack  = snd $ applyOp x xs
                retHead   | (length retStack) > 0 = head retStack
                          | otherwise = error $ "applyOp returned nothing for "++x
                retTail   | (length retStack) > 1 = tail $ fst $ applyOp x xs
                          | otherwise = []
        helper source@(Tape _ _ []) = source
        helper source@(Tape _ _ _) = helper $ moveRight source
