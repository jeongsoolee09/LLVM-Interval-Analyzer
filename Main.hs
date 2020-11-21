module Main where

import Fixpoint
import Program
import Domain

import LLVM.Module
import LLVM.Internal.Module
import LLVM.Internal.Context
import LLVM.AST
import Options.Applicative
import Data.Semigroup ((<>))

onePass :: CFG -> IO ()
onePass cfg =
      let table = analyze cfg in do
        putStrLn $ "Analysis of " ++ (getFid cfg) ++ " begins...\n"
        putStrLn ""
        putStrLn $ "Analysis Results for: " ++ (getFid cfg)
        putStrLn (tableToString table)
        putStrLn ""


multiPass :: [CFG] -> IO ()
multiPass [] = return ()
multiPass (cfg:cfgs) = do
  onePass cfg >> multiPass cfgs


data Options = Options {filePath :: String}


getFilePath :: Options -> String
getFilePath (Options filePath) = filePath

  
main :: IO ()
main = do
  args <- execParser $ info (Options <$>
                             strOption (long "input" <>
                                        short 'i' <>
                                        help "Path to the input file.") <*
                             abortOption ShowHelpText (long "help" <>
                                                       short 'h' <>
                                                       help "Display this message."))
          (progDesc "Intraprocedural sign analyzer." <> fullDesc)
  rawModule <- readFile (getFilePath args)
  pureModule <- withContext $ \context -> withModuleFromLLVMAssembly context rawModule moduleAST
  let myModule = Program.newModule pureModule in
    multiPass (getCFGs myModule)
