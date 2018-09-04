module Main where
import Text.Parsec hiding (spaces,State)
import Text.Parsec.String
import Data.Maybe
import qualified Data.Map as Map
import Tape
import Debug.Trace
import Control.Monad.State
import System.Environment
import System.Console.Haskeline

main :: IO ()
main = do options <- getArgs
          case options of
            []                 -> runInputT defaultSettings $ replMode False Map.empty
            ("-v":_)           -> runInputT defaultSettings $ replMode True Map.empty
            ("-f":fileName:_)  -> runInputTBehavior (useFile fileName) defaultSettings
                                  $ fileMode False Map.empty 1 >> return ()
            ("-vf":fileName:_) -> runInputTBehavior (useFile fileName) defaultSettings
                                  $ fileMode True Map.empty 1 >> return ()
            _                  -> putStrLn "I don't understand your arguments..."

type FunMap = Map.Map String SfFun

replMode :: Bool -> FunMap -> InputT IO ()
replMode verbose sfFuns = do
  a <- getInputLine "((λ)): "
  case a of
    Nothing           -> return ()
    Just ""           -> replMode verbose sfFuns
    Just ('#':_)      -> replMode verbose sfFuns
    Just (':':'q':_)  -> return ()
    Just (':':'d':_)  -> replMode (not verbose) sfFuns
    Just (':':'m':_)  -> do let s = Map.lookup (head $ tail $ words $ fromJust a) sfFuns 
                            case s of
                              Nothing    -> outputStrLn "I don't know this function..."
                              Just sfFun -> outputStrLn (showFun (head $ tail $ words $ fromJust a) sfFun)
                            replMode verbose sfFuns
    Just (':':'l':_)  -> do (_,newFuns) <- runInputTBehavior
                                             (useFile $ head $ tail $ words $ fromJust a)
                                             defaultSettings
                                             $ fileMode verbose sfFuns 1
                            outputStrLn("done loading!") --TODO (Documentation)
                            replMode verbose newFuns
    Just input    -> do let (out,newFuns) = runState (runLine verbose input 1) sfFuns
                        outputStrLn out
                        replMode verbose newFuns

fileMode :: (MonadException m) => Bool -> FunMap -> Int -> InputT m (String,FunMap)
fileMode verbose sfFuns counter = do
  a <- getInputLine ""
  case a of
    Nothing      -> return ("",sfFuns) 
    Just ""      -> fileMode verbose sfFuns (counter+1)
    Just ('#':_) -> fileMode verbose sfFuns (counter+1)
    Just input   -> do let (b,c) = runState (runLine verbose input counter) sfFuns
                       if (length b) > 1
                         then outputStrLn b >> return (b,sfFuns)
                         else fileMode verbose c (counter+1)
  
runLine :: Bool -> String -> Int -> State FunMap String
runLine verbose newLine lineNum = do
  code <- runHelper
  if code == mempty
    then do return ""
    else return $ show $ code
  where parseHelper input = parse sourceParser ("") input
        runHelper = case parseHelper newLine of
          Left err -> trace ("I ran into an error while parsing (line " ++ (show lineNum) ++", "
                             ++ (drop 9 $ show $ err)) $ return mempty
          Right val -> sfRun verbose val

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
pop (SfStack []) = error "You tried to pop an empty stack..."
pop (SfStack xs) = head xs

popped :: SfStack -> SfStack
popped (SfStack []) = error "You tried to popped an empty stack..."
popped (SfStack xs) = SfStack $ tail xs

isEmpty :: SfStack -> Bool
isEmpty (SfStack []) = True
isEmpty _ = False

instance Monoid SfStack where
  mempty = SfStack []
  mappend (SfStack xs) (SfStack ys) = SfStack $ xs++ys

data SfTail = SfTail SfStack SfStack -- condition and return stack

instance Show SfTail where
  show (SfTail (SfStack []) bs) = "? " ++ (show bs)
  show (SfTail as           bs) = "? " ++ (show as) ++  ": " ++ (show bs) 

getCond (SfTail cond _) = cond
getBody (SfTail _ body) = body

data SfFun  = SfFun [SfToken] [SfTail] deriving (Show) -- args and tail  

showFun _ (SfFun [] [])    = ""
showFun name (SfFun [] xs) = name ++ " : " ++ (drop 2 $ concatMap show xs)
showFun name (SfFun as xs) = (concatMap show as) ++ name ++ " " ++ (concatMap show xs)

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

reservedCharacter = (oneOf "+-*/<=^v°$§:;?") <?> "reserved Character"

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
                      return $ [SfTail mempty a] -- one stack without condition

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
sourceParser = do a <- try declParser <|> try mainStackParser
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
applyBuiltIn a (_) = error $ "built-in function applied to wrong arguments " ++ [a]


applyFun :: String -> [SfToken] -> FunMap -> ([SfToken], [SfToken])
applyFun name xs funMap = helper fun
  where fun = if isJust f then fromJust f else error $ "I never heard of " ++ name
                  where f = Map.lookup name funMap
        helper fun@(SfFun fArgs _) = (drop (length fArgs) xs,
                                      replBody fArgs (findBody fun xs) xs)
        findBody (SfFun _ []) _ = error $ "tried to apply empty function " ++ name
        findBody (SfFun _ (fTail:[])) xs = getBody fTail
        findBody (SfFun fArgs (fTail:ts)) xs
          | isEmpty $ getCond fTail = getBody fTail
          | condReturn == Boolean True = getBody fTail
          | condReturn == Boolean False =  findBody (SfFun fArgs ts) xs
          | otherwise = error $ "condition didn't return boolean " ++ (show $ getCond fTail)
                                ++ " " ++ (show $ condReturn)
          where condReturn = (pop $ sfEval False (SfStack $ replBody fArgs (getCond fTail) xs) funMap)
        replBody fArgs (SfStack body) args | length args >= length fArgs =
                                             [lookAndExchange x $ zip (reverse fArgs) args | x <- body]
                                           | otherwise = error $ "this function needs more arguments: "
                                                         ++ name
          where lookAndExchange (Stack x) table = Stack $ SfStack $ replBody fArgs x args
                lookAndExchange x table = fromMaybe x $ lookup x table


sfEval :: Bool -> SfStack -> FunMap -> SfStack
sfEval verbose (SfStack x) funMap = SfStack $ tapeToList $ helper (listToTape x) funMap
  where helper source@(Tape xs (BuiltIn x) ys) funMap
          | isJust retHead = traceShowIf verbose source $
                             helper (Tape xsMinArgs (fromJust retHead) (retTail++ys)) funMap
          | ys /= []       = traceShowIf verbose source $
                             helper (Tape xsMinArgs (head ys) (tail ys)) funMap
          | otherwise      = traceShowIf verbose source $
                             (Tape xsMinArgs (Identifier "") [])
          where xsMinArgs = fst $ applyBuiltIn x xs
                retStack  = snd $ applyBuiltIn x xs
                retHead   | (length retStack) > 0 = Just $ head retStack
                          | otherwise = Nothing
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []

        helper source@(Tape xs (Identifier x) ys) funMap
          | isJust retHead = traceShowIf verbose source $
                             helper (Tape xsMinArgs (fromJust retHead) (retTail++ys)) funMap
          | ys /= []       = traceShowIf verbose source $
                             helper (Tape xsMinArgs (head ys) (tail ys)) funMap
          | otherwise      = traceShowIf verbose source $
                             (Tape xsMinArgs (Identifier "") [])
          where xsMinArgs = fst $ applyFun x xs $ funMap
                retStack  = snd $ applyFun x xs $ funMap
                retHead   | (length retStack) > 0 = Just $ head retStack
                          | otherwise = Nothing
                retTail   | (length retStack) > 1 = tail retStack
                          | otherwise = []

        helper source@(Tape _ _ []) _ = traceShowIf verbose source $ source
        helper source@(Tape _ _ _) funMap = traceShowIf verbose source $ helper (moveRight source) funMap


sfRun :: Bool -> SfSource -> State FunMap SfStack
sfRun _ (Fun ((Identifier x),y)) = do
  sfFuns <- get
  put $ Map.insert x y sfFuns
  return mempty
  
sfRun verbose (MainStack x) = do
  gets (sfEval verbose x)

sfRun _ x = error $ "This isn't propperly formatted sofun code: " ++ (show x)

traceShowIf True  x = traceShow x
traceShowIf False _ = id

traceIf True  x = trace x
traceIf False _ = id
