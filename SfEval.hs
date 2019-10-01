module SfEval where

import           Control.Monad.State
import qualified Data.Map            as Map
import           Data.Maybe
import           Debug.Trace
import           SfParser
import           System.IO.Unsafe

runLine :: Bool -> String -> Int -> State FunMap String
runLine verbose newLine lineNum = do
    code <- runHelper
    if code == mempty
        then return ""
        else return $ show code
  where
    runHelper =
        case parseSofun newLine of
            Left err ->
                trace ("error while parsing (line " ++ show lineNum ++ ", " ++ drop 9 (show err)) $
                return mempty
            Right val -> sfRun verbose val

getArgNum :: SfToken -> FunMap -> Int
getArgNum (BuiltIn x) funMap
    | x == ',' = 0
    | x `elem` "v;°." = 1
    | x `elem` "+-*/%<==^" = 2
    | otherwise = error $ "unkown builtin " ++ [x]
getArgNum (Identifier x) funMap
    | isJust f = getFArgNum $ fromJust f
    | otherwise = 0
  where
    f = Map.lookup x funMap
    getFArgNum (SfFun args _) = length args

applyBuiltIn :: Bool -> Char -> [SfToken] -> FunMap -> [SfToken]
-- IO no arg
applyBuiltIn _ ',' xs _ = readString : xs
  where
    readString :: SfToken
    readString = Stack $ foldl (flip push) (SfStack []) (map Character $ unsafePerformIO getLine)
--first arg is a function
applyBuiltIn v x source@(BuiltIn y:ys) m = applyBuiltIn v x (sfEval v (SfStack source) 1 m) m
applyBuiltIn v x source@(Identifier y:ys) m = applyBuiltIn v x (sfEval v (SfStack source) 1 m) m
--IO one arg
applyBuiltIn _ '.' (Stack y:xs) _ = printString "" y ++ xs
  where
    printString :: String -> SfStack -> [SfToken]
    printString acc (SfStack []) = unsafePerformIO $ putStrEmptyList acc
    printString acc (SfStack (Character x:xs)) = printString (x : acc) (SfStack xs)
    printString _ _ = []
    putStrEmptyList x = do
        putStrLn x
        return []
--stack one arg
applyBuiltIn _ 'v' (Stack y:xs) _ = pop y : xs
applyBuiltIn _ ';' (Stack y:xs) _ = (Stack $ popped y) : xs
applyBuiltIn _ '°' (Stack y:xs) _ = (Boolean $ isEmpty y) : xs
--second arg is a function
applyBuiltIn v x source@(y:BuiltIn z:zs) m = applyBuiltIn v x (sfEval v (SfStack source) 2 m) m
applyBuiltIn v x source@(y:Identifier z:zs) m = applyBuiltIn v x (sfEval v (SfStack source) 2 m) m
-- arithmetics two args
applyBuiltIn _ '+' (Number x:Number y:xs) _ = (Number $ y + x) : xs
applyBuiltIn _ '-' (Number x:Number y:xs) _ = (Number $ y - x) : xs
applyBuiltIn _ '*' (Number x:Number y:xs) _ = (Number $ y * x) : xs
applyBuiltIn _ '/' (Number x:Number y:xs) _ = (Number $ y / x) : xs
applyBuiltIn _ '%' (Number x:Number y:xs) _ = (Number $ fromIntegral $ mod (floor x) (floor y)) : xs
-- logic two args
applyBuiltIn _ '<' (Number x:Number y:xs) _ = (Boolean $ y < x) : xs
applyBuiltIn _ '=' (Number x:Number y:xs) _ = (Boolean $ x == y) : xs
applyBuiltIn _ '=' (Boolean x:Boolean y:xs) _ = (Boolean $ x == y) : xs
-- stack two args
applyBuiltIn _ '^' (x@(Number _):Stack y:xs) _ = (Stack $ push x y) : xs
applyBuiltIn _ '^' (x@(Character _):Stack y:xs) _ = (Stack $ push x y) : xs
applyBuiltIn _ '^' (x@(Stack _):Stack y:xs) _ = (Stack $ push x y) : xs
applyBuiltIn _ a xs _ =
    error $
    "built-in function \'" ++ [a] ++ "\' applied to wrong arguments, args: " ++ show (SfStack xs)

applyFun :: Bool -> String -> [SfToken] -> FunMap -> [SfToken]
applyFun v name source funMap = helper fun
  where
    fun = fromMaybe (error $ "I never heard of " ++ name) f
      where
        f = Map.lookup name funMap
    argNum (SfFun fArgNames _) = length fArgNames
    argList = sfEval v (SfStack source) (argNum fun) funMap
    helper fun@(SfFun fArgNames _) = replBody fArgNames (findBody fun) ++ drop (argNum fun) argList
    findBody (SfFun _ []) = error $ "tried to apply empty function " ++ name
    findBody (SfFun _ [fTail]) = getBody fTail
    findBody (SfFun fArgNames (fTail:ts))
        | isEmpty $ getCond fTail = getBody fTail
        | condReturn == Boolean True = getBody fTail
        | condReturn == Boolean False = findBody (SfFun fArgNames ts)
        | otherwise =
            error $
            "condition didn't return boolean " ++ show (getCond fTail) ++ " " ++ show condReturn
      where
        condReturn = head $ sfEval False (SfStack $ replBody fArgNames (getCond fTail)) 1 funMap
    replBody fArgNames (SfStack body)
        | length (take (argNum fun) argList) >= argNum fun -- TODO is this faster as comparing the lengths?
         = [lookAndExchange x $ zip (reverse fArgNames) argList | x <- body]
        | otherwise = error $ "this function needs more arguments: " ++ name
      where
        lookAndExchange (Stack x) table = Stack $ SfStack $ replBody fArgNames x
        lookAndExchange x table = fromMaybe x $ lookup x table

sfEval :: Bool -> SfStack -> Int -> FunMap -> [SfToken]
sfEval v (SfStack stack) 0 funMap = stack
sfEval v (SfStack stack) depth funMap = traceShowIf v (SfStack stack) $ helper stack
  where
    helper stack@(BuiltIn x:xs) =
        sfEval
            v
            (SfStack $
             applyBuiltIn
                 v
                 x
                 (sfEval v (SfStack xs) (max (depth - 1) $ getArgNum (BuiltIn x) funMap) funMap)
                 funMap)
            1
            funMap
    helper stack@(Identifier x:xs) =
        sfEval
            v
            (SfStack $
             applyFun
                 v
                 x
                 (sfEval v (SfStack xs) (max (depth - 1) $ getArgNum (Identifier x) funMap) funMap)
                 funMap)
            1
            funMap
    helper (x:xs) = x : sfEval v (SfStack xs) (depth - 1) funMap
    helper [] = []

sfRun :: Bool -> SfSource -> State FunMap SfStack
sfRun _ (Fun (Identifier x, y)) = do
    sfFuns <- get
    put $ Map.insert x y sfFuns
    return mempty
sfRun verbose (MainStack (SfStack x)) = gets (SfStack . sfEval verbose (SfStack x) 1)
sfRun _ x = error $ "This isn't propperly formatted sofun code: " ++ show x

traceShowIf True x  = traceShow x
traceShowIf False _ = id

traceIf True x  = trace x
traceIf False _ = id
