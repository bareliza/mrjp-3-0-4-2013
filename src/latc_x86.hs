import System.Environment ( getArgs, getProgName )
import System.IO ( stdin, stderr, hPutStrLn )
import System.Exit (exitSuccess, exitFailure, exitWith)
import System.Process ( system )

import LexLatte
import ParLatte
import SkelLatte
import PrintLatte
import AbsLatte

import ErrM

import X86c

import Control.Monad
import Data.List

myLLexer = myLexer

parseSth parser s = let ts = myLLexer s in 
  if ts /= [] then parser ts else Bad "Blad skladni: nie rozpoznano tokenow."

getTopDefs (Program topdefs) = topdefs

checkSth :: FilePath -> IO ()
checkSth f = do 
  kod <- readFile f
  bledy <- return (parseSth pProgram kod)
  putStrLn (show bledy)

-- Parsowanie i Wejście Wyjście

x86c :: FilePath -> IO ()
x86c f = do
  kod <- readFile f
  tree <- return (parseSth pProgram kod)
  case tree of
    Bad _ -> return ()
    Ok tree1 -> 
      do
        writeFile (replaceExtWithDotS f) (compileProgram tree1) 
        system 
          ("gcc -m32 -g -o "++(pathToFile f)++"a.out "++
           (replaceExtWithDotS f)++" runtime.o")
        return ()
          

findLastIdx :: Char -> String -> Int
findLastIdx c s = case findIndices (\a -> a == c) s of
  [] -> -1
  lst -> last lst 

pathToFile :: FilePath -> String
pathToFile s =
  case findLastIdx '/' s of
    -1 -> "./"
    slash -> take (slash+1) s

replaceExtWithDotS :: FilePath -> String
replaceExtWithDotS s =
  case findLastIdx '.' s of
    -1 -> s
    dot -> (take dot s)++".s"

main :: IO ()
main = do 
  args <- getArgs
  case args of
    [filePath] -> x86c filePath 
    _ -> putStrLn ("Uzycie:\n"++
                   "\n"++
                   "latc_x86 plikZrodlowy - kompilacja do asemblera x86.")
