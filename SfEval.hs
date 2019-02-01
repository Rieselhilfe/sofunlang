module SfEval where
import Data.Maybe
import qualified Data.Map as Map
import Debug.Trace
import Control.Monad.State
import System.IO.Unsafe
import Tape
import SfParser

runLine :: Bool -> String -> Int -> State FunMap String
runLine verbose newLine lineNum = do
  code <- runHelper
  if code == mempty
    then do return ""
    else return $ show $ code
  where 
        runHelper = case parseSofun newLine of
          Left err -> trace ("error while parsing (line " ++ (show lineNum) ++", "
                             ++ (drop 9 $ show $ err)) $ return mempty
          Right val -> sfRun verbose val

applyBuiltIn :: Char -> [SfToken] -> ([SfToken], [SfToken])
-- arithmetics
applyBuiltIn '+' ((Number x):(Number y):xs)      = (xs,[Number $ y+x])
applyBuiltIn '-' ((Number x):(Number y):xs)      = (xs,[Number $ y-x])
applyBuiltIn '*' ((Number x):(Number y):xs)      = (xs,[Number $ y*x])
applyBuiltIn '/' ((Number x):(Number y):xs)      = (xs,[Number $ y/x])
applyBuiltIn '%' ((Number x):(Number y):xs)      = (xs,[Number $
                                                        fromIntegral $
                                                        mod (floor $ x) (floor $ y)])
-- logic
applyBuiltIn '<' ((Number x):(Number y):xs)      = (xs,[Boolean $ y<x])
applyBuiltIn '=' ((Number x):(Number y):xs)      = (xs,[Boolean $ x==y])
applyBuiltIn '=' ((Boolean x):(Boolean y):xs)    = (xs,[Boolean $ x==y])
-- stack
applyBuiltIn '^' (x@(Number _):(Stack y):xs)    = (xs,[Stack $ push x y])
applyBuiltIn '^' (x@(Character _):(Stack y):xs) = (xs,[Stack $ push x y])
applyBuiltIn '^' (x@(Stack _):(Stack y):xs)     = (xs,[Stack $ push x y])
applyBuiltIn 'v' ((Stack y):xs)                  = (xs,[pop y])
applyBuiltIn ';' ((Stack y):xs)                  = (xs,[Stack $ popped y])
applyBuiltIn '°' ((Stack y):xs)                  = (xs,[Boolean $ isEmpty y])
-- IO
applyBuiltIn '.' ((Stack y):xs)                  = (xs,printString "" y)
  where printString :: String -> SfStack -> [SfToken]
        printString acc (SfStack []) = unsafePerformIO $ putStrEmptyList acc
        printString acc (SfStack ((Character x):xs)) = printString (x:acc) (SfStack xs)
        printString _ _ = []
        putStrEmptyList x = do putStrLn x
                               return []
applyBuiltIn ',' (xs)                          = (xs,[readString])
  where readString :: SfToken
        readString = Stack $ foldl (flip push) (SfStack []) (map Character $ unsafePerformIO $ getLine)
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
