{-# LANGUAGE OverloadedStrings #-}

module Lib (
    someFunc,
) where

import Control.Concurrent
import Control.Monad (forever)
import Data.Function ((&))
import qualified Data.Text as T
import GHC.IORef
import Rainbow
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
printColoredEvent _ = return ()

printReservantLn :: String -> IO ()
printReservantLn x = do
    putChunk $ "[RESERVANT] " & fore yellow
    putStrLn x

someFunc :: FilePath -> IO ()
someFunc path = do
    withManager $ \mgr -> do
        threadId <- forkIO $ callCommand $ "stack runhaskell " ++ path

        threadIdIORef <- newIORef threadId

        _ <-
            watchTree
                mgr
                "./src"
                predicate
                ( \event -> do
                    printColoredEvent event

                    case event of
                        CloseWrite{} -> do
                            printReservantLn "Stopping the server"

                            currentThreadId <- readIORef threadIdIORef

                            killThread currentThreadId

                            printReservantLn "Building the program..."

                            callCommand "stack build"

                            printReservantLn "Building done"

                            printReservantLn "Starting server..."

                            newThreadId <- forkIO $ callCommand "stack runhaskell app/Main.hs"

                            writeIORef threadIdIORef newThreadId

                            return ()
                        _ -> return ()
                )
        forever $ threadDelay 1000000
