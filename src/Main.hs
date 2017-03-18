module Main where

import System.Directory
import Data.List
import Debug.Trace

data Tree = File FilePath | Directory FilePath [Tree]
  deriving Show

exampleTree = Directory "." [Directory "src" [File "Hello.hs"], File "hello.cabal"]

--print :: Show a => a -> IO ()
--print = putStrLn . show

-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- a >> b = a >>= \_ -> b
-- return :: a -> IO a

toTrees :: FilePath -> [FilePath] -> IO [Tree]
toTrees _ [] = return []
toTrees context (path:paths) =
  toTree context path >>= \x ->
  toTrees context paths >>= \xs ->
  return (x:xs)

toTree :: FilePath -> FilePath -> IO Tree
toTree context fp = doesDirectoryExist (context++fp) >>= \isDir ->
  case isDir of
    True ->
      putStrLn ("Looking at " ++ fp ++ " in " ++ context) >>
      listDirectory (context++fp) >>= \list ->
      toTrees (context++fp++"/") list >>= \x ->
      return (Directory fp x)
    False -> return (File fp)

prettyPrint' :: Int -> Tree -> String
prettyPrint' i (File fp) = replicate (2*i) ' ' ++ fp
prettyPrint' i (Directory fp trees)
  = replicate (2*i) ' ' ++ fp ++ "/\n"
  ++ intercalate "\n" (map (prettyPrint' (i+1)) trees)

prettyPrint :: Tree -> String
prettyPrint = prettyPrint' (sideEffects 0)

sideEffects :: Show a => a -> a
sideEffects x = trace ("Purity is a lie. " ++ show x) x

main :: IO ()
main = toTree "./" "." >>= putStrLn . prettyPrint
