module Main where
import Data.Maybe
import qualified Data.Map as Map
import Data.Char (isSpace) 
import Debug.Trace
import SfEval
import SfParser
import Control.Monad.State
import System.Environment
import System.Console.Haskeline

main :: IO ()
main = do options <- getArgs
          (_,stdFuns) <- runInputTBehavior
                         (useFile "std.fun") --maybe make directory not hardcoded
                         defaultSettings
                         $ fileMode False Map.empty 1
          if (length options) <= 2
            then helper options stdFuns
            else putStrLn "I don't expect that much arguments"
            where helper args stdFuns =
                    case args of
                    []                 -> runInputT defaultSettings $ replMode False stdFuns
                    ("-v":_)           -> runInputT defaultSettings $ replMode True stdFuns
                    ("-f":fileName:_)  -> runInputTBehavior (useFile fileName) defaultSettings
                                          $ fileMode False stdFuns 1 >> return ()
                    ("-vf":fileName:_) -> runInputTBehavior (useFile fileName) defaultSettings
                                          $ fileMode True stdFuns 1 >> return ()
                    (fileName:_)       -> runInputTBehavior (useFile fileName) defaultSettings
                                          $ fileMode False stdFuns 1 >> return ()

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
                        if (all isSpace out)
                          then outputStr ""
                          else outputStrLn out
                        replMode verbose newFuns

fileMode :: (MonadException m) => Bool -> FunMap -> Int -> InputT m (String,FunMap)
fileMode verbose sfFuns counter = do
  a <- getInputLine ""
  case a of
    Nothing      -> return ("",sfFuns) 
    Just ""      -> fileMode verbose sfFuns (counter+1)
    Just ('#':_) -> fileMode verbose sfFuns (counter+1)
    Just (':':'l':_)  -> do (_,newFuns) <- runInputTBehavior
                                             (useFile $ head $ tail $ words $ fromJust a)
                                             defaultSettings
                                             $ fileMode verbose sfFuns 1
                            fileMode verbose newFuns (counter+1)
    Just input   -> do let (b,c) = runState (runLine verbose input counter) sfFuns
                       if (length b) > 1
                         then outputStrLn b >> return (b,sfFuns)
                         else fileMode verbose c (counter+1)
