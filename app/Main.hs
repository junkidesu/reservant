module Main (main) where

import Lib
import Options.Applicative

newtype MainModulePath = MainModulePath FilePath
        deriving (Read)

newtype Options = Options
        { mainModulePath :: MainModulePath
        }

mainModulePathParser :: Parser MainModulePath
mainModulePathParser =
        MainModulePath
                <$> strArgument
                        ( help "The path of the module containing the main method"
                                <> metavar "path"
                        )
optionsParser :: Parser Options
optionsParser = Options <$> mainModulePathParser

main :: IO ()
main = run =<< execParser opts
    where
        opts =
                info
                        (optionsParser <**> helper)
                        ( fullDesc
                                <> progDesc "Edit your Haskell project, and let reservant re-build and restart it for you"
                                <> header "reservant-exe - Nodemon clone for Haskell"
                        )

run :: Options -> IO ()
run opts =
        let (MainModulePath path) = mainModulePath opts
         in someFunc path
