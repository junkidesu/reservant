{-# LANGUAGE OverloadedStrings #-}

module Lib (
    startReservant,
) where

import Control.Concurrent
import Control.Monad (forever)
import Data.Function ((&))
import qualified Data.Text as T
import GHC.IORef
import Rainbow
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FSNotify
import System.Process

predicate :: Event -> Bool
predicate (Added{}) = True
predicate (Modified{}) = True
predicate (Removed{}) = True
predicate (CloseWrite{}) = True
predicate _ = False

printColoredEvent :: Event -> IO ()
printColoredEvent added@(Added{}) = do
    putChunk $
        "[ADDED] " & fore green
    putChunk $ (chunk . T.pack . show . eventTime $ added) <> " " & italic & fore grey
    putStrLn $ eventPath added
printColoredEvent modified@(Modified{}) = do
    putChunk $
        "[MODIFIED] " & fore blue
    putChunk $ (chunk . T.pack . show . eventTime $ modified) <> " " & italic & fore grey
    putStrLn $ eventPath modified
printColoredEvent removed@(Removed{}) = do
    putChunk $
        "[REMOVED] " & fore red
    putChunk $ (chunk . T.pack . show . eventTime $ removed) <> " " & italic & fore grey
    putStrLn $ eventPath removed
printColoredEvent written@(CloseWrite{}) = do
    putChunk $
        "[WRITTEN] " & fore yellow
    putChunk $ (chunk . T.pack . show . eventTime $ written) <> " " & italic & fore grey
    putStrLn $ eventPath written
printColoredEvent _ = return ()

printLog :: String -> IO ()
printLog s = do
    putChunk $ "[RESERVANT] " & fore yellow
    putStrLn s

printError :: String -> IO ()
printError s = do
    putChunk $ "[ERROR] " & fore red
    putStrLn s

buildAndStart :: IORef (Maybe ThreadId) -> FilePath -> IO ()
buildAndStart threadIdIORef path = do
    printLog "Building the program..."

    buildProcessHandle <- runCommand "stack build"

    buildResult <- waitForProcess buildProcessHandle

    case buildResult of
        ExitFailure _ -> do
            printError "Failed to build the project"
        ExitSuccess -> do
            printLog "Starting server... Done."

            threadId <- forkIO $ callCommand $ "stack runhaskell " ++ path

            writeIORef threadIdIORef (Just threadId)

startReservant :: FilePath -> IO ()
startReservant path = do
    withManager $ \mgr -> do
        threadIdIORef <- newIORef (Nothing :: Maybe ThreadId)

        buildAndStart threadIdIORef path

        _ <-
            watchTree
                mgr
                "./src"
                predicate
                ( \event -> do
                    printColoredEvent event

                    case event of
                        -- build and restart the application if the file is written
                        CloseWrite{} -> do
                            mbCurrentThreadId <- readIORef threadIdIORef

                            case mbCurrentThreadId of
                                Nothing -> return ()
                                Just currentThreadId -> do
                                    printLog "Stopping the server"
                                    killThread currentThreadId

                            buildAndStart threadIdIORef path

                        -- ignore if the file is not written
                        _ -> return ()
                )
        forever $
            threadDelay 1000000
