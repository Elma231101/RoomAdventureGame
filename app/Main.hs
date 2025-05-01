module Main where

import World
import Game
import Commands
import System.IO

play :: State -> IO ()
play state = do
    putStr "\n> "
    hFlush stdout
    input <- getLine
    let (state', mesg) = parseInput state input
    let player = getPlayer state'
    if getHealth player < 1
        then do
            putStrLn "WASTED! Health dropped below zero. Restart the game."
            return ()
    else if getHealth player < 11
        then do
            putStrLn mesg
            putStrLn "Critical Health level! Go eat something to get healthy again."
            play state'
    else if mesg == "Quit"
        then do
            putStrLn "Exiting the program. Thank you for playing!"
            return ()
    else do
        putStrLn mesg
        play state'

main :: IO ()
main = play state
