module Main where

import Analyze
import Program
import Domain

import LLVM.Module
import LLVM.Internal.Module
import LLVM.Internal.Context
import LLVM.AST
import Options.Applicative
import Data.Semigroup ((<>))

onePass :: [CFG] -> CFG -> IO ()
onePass cfgPool cfg =
      let table = analyze cfgPool cfg in do
        putStrLn $ "Analysis of " ++ (getFid cfg) ++ " begins...\n"
        putStrLn ""
        putStrLn $ "Analysis Results for: " ++ (getFid cfg)
        putStrLn (tableToString table)
        putStrLn ""


multiPass :: [CFG] -> [CFG] -> IO ()
multiPass _ [] = return ()
multiPass cfgPool (cfg:cfgs) = do
  onePass cfgPool cfg >> multiPass cfgPool cfgs


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
    multiPass (getCFGs myModule) (getCFGs myModule)
