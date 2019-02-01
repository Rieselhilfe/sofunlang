module SfParser where
import Text.Parsec hiding (spaces,State)
import Text.Parsec.String
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace

parseSofun input = parse sourceParser ("") input

type FunMap = Map.Map String SfFun

data SfToken = Number Double
             | Boolean Bool
             | BuiltIn Char
             | Identifier String
             | Stack SfStack
             | Character Char
             | Argument Int
             deriving (Eq)

isString :: SfStack -> Bool
isString (SfStack []) = True
isString (SfStack ((Character _):xs)) = isString $ SfStack xs
isString _ = False

instance Show SfToken where
  show (Number x) = (show x)++" "
  show (Identifier x) = x++" "
  show (Stack (SfStack x)) | isString (SfStack x) = "\""++(map (head . (drop 1) . show) $ reverse x)++"\""
                           | otherwise =  "( "++(concatMap show $ reverse x)++") "
  show (Boolean x) | x == True = "§ "
                   | otherwise = "$ "
  show (BuiltIn x) = x:" "
  show (Character x) = "\'" ++ [x] ++ "\' "
  show (Argument x) = "%%x"++(show x)++"%%"

data SfStack = SfStack [SfToken]
                 deriving (Eq)

instance Show SfStack where
  show (SfStack x) = concatMap show x

instance Semigroup SfStack where
  (<>) (SfStack xs) (SfStack ys) = SfStack $ xs++ys

instance Monoid SfStack where
  mempty = SfStack []
  mappend (SfStack xs) (SfStack ys) = SfStack $ xs++ys

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

lookAndExchange :: [(SfToken, SfToken)] -> SfToken -> SfToken
lookAndExchange table (Stack (SfStack x)) = Stack $ SfStack $ map (lookAndExchange table) x
lookAndExchange table x = fromMaybe x $ lookup x table

spaces :: Parser ()
spaces = skipMany1 space

comment :: Parser ()
comment = do char '#'
             return ()

specialCharacter :: Parser Char
specialCharacter = (oneOf "!>@_{[]}´`'\"\\~&|ł€ŧ←↓→øþſæđŋħĸł»«¢„“”µ·…")
                   <?> "special character"

reservedCharacter :: Parser Char
reservedCharacter = (oneOf "+-*/<=^v°$§:;?.,") <?> "reserved Character"

builtInParser :: Parser SfToken
builtInParser = do symbol <- oneOf "+-*/<;=^%v°.," <?> "built in"
                   spaces <|> eof <|> comment
                   return $ BuiltIn symbol

reservedButLonger :: Parser Char
reservedButLonger = do a <- reservedCharacter
                       lookAhead $ many1 (alphaNum <|> reservedCharacter)
                       return $ a

identifierParser :: Parser SfToken
identifierParser = do first <- (letter <|> reservedButLonger <|> specialCharacter)
                      rest <- many (letter <|> digit <|> specialCharacter
                                    <|> reservedCharacter)
                      spaces <|> eof <|> comment
                      return $ Identifier (first:rest)

floatParser :: Parser SfToken
floatParser = do dash <- option "" $ (string "-" <|> (char '+' >> return ""))
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

charParser :: Parser SfToken
charParser = do char '\''
                a <- noneOf "\'\""
                char '\''
                spaces <|> eof <|> comment
                return $ Character a

stringParser :: Parser SfToken
stringParser = do char '\"'
                  a <- many $ noneOf "\'\""
                  char '\"'
                  spaces <|> eof <|> comment
                  return $ Stack $ SfStack $ reverse $ map Character a

allSimpleTokenParser :: Parser SfStack
allSimpleTokenParser = do
  a <- many (try floatParser <|> try booleanParser <|> try builtInParser <|>
             try charParser <|> try stringParser <|> try identifierParser <|>
             try stackParser)
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
                return $ Fun $ ((last as),
                                SfFun (numberList $ init as)
                                (exchangeInTail (numberAssList $ init as) b))
   where numberList = (map (Argument . fst) . zip [1,2..])
         numberAssList = (map (\x -> (snd x, Argument $ fst x)) . zip [1,2..])
         exchangeInTail nAL b =
           [SfTail (SfStack (map (lookAndExchange $ nAL) cond))
                   (SfStack (map (lookAndExchange $ nAL) body))
           | (SfTail (SfStack cond) (SfStack body)) <- b]  

sourceParser :: Parser SfSource
sourceParser = do a <- try declParser <|> try mainStackParser
                  eof <|> comment
                  return $ a
