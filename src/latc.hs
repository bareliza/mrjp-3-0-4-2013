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

import Environment
import Semantics
import qualified JVMc as JVMc

import Control.Monad

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

checkFile :: FilePath -> IO ()
checkFile f = do 
  kod <- readFile f
  tree <- return (parseSth pProgram kod)
  bledy <- return (case tree of
                      Bad s -> [s]
                      Ok tree1 -> checkProgram tree1)
  if bledy == [] then hPutStrLn stderr "OK\n" else hPutStrLn stderr "ERROR\n"
  forM_ bledy (hPutStrLn stderr)
  case tree of
    Bad _ -> return ()
    Ok tree1 -> putStrLn (show tree1)
  if bledy == [] then exitSuccess else exitFailure

jvmc :: FilePath -> IO ()
jvmc f = do
  kod <- readFile f
  tree <- return (parseSth pProgram kod)
  bledy <- return (case tree of
                      Bad s -> [s]
                      Ok tree1 -> checkProgram tree1)
  if bledy == [] then hPutStrLn stderr "OK\n" else hPutStrLn stderr "ERROR\n"
  forM_ bledy (hPutStrLn stderr)
  case tree of
    Bad _ -> return ()
    Ok tree1 -> 
      if bledy == [] then 
        do
          writeFile (replaceExtWithDotj f) (JVMc.jvmc f tree1) 
          system ("java -jar lib/jasmin.jar -d "++(pathToFile f)++" "++
            (replaceExtWithDotj f))
          return ()
      else return ()
  if bledy == [] then exitSuccess else exitFailure

pathToFile :: FilePath -> String
pathToFile s =
  case JVMc.findLastIdx '/' s of
    -1 -> "./"
    slash -> take (slash+1) s

replaceExtWithDotj :: FilePath -> String
replaceExtWithDotj s =
  case JVMc.findLastIdx '.' s of
    -1 -> s
    dot -> (take dot s)++".j"

main :: IO ()
main = do 
      args <- getArgs
      case args of
        ("-a":fs) ->  checkFile $ head fs
        [filePath] -> jvmc filePath 
        _ -> putStrLn ("Uzycie:\n"++
             "\n"++
             "latc -a plikZrodlowy - kontrola skladniowa\n"++
             "latc plikZrodlowy - kompilacja do jvm."++
             "Nalezy uruchamiac w katalogu zawierajacym program latc.\n")
