module Main where

import World
import Game
import Commands
import System.IO

-- Function to play the game, taking the current state as an argument
play :: State -> IO ()
play state = do
    putStr "\n> " -- Prompt the user for input
    hFlush stdout -- Ensure the prompt is displayed immediately
    input <- getLine -- Read a line of input from the user
    let (state', mesg) = parseInput state input -- Parse the input and get the new state and message
    let player = getPlayer state' -- Get the player from the updated state
    -- end the game if health drops below 1
    if getHealth player < 1
        then do
            putStrLn "WASTED! Health dropped below zero. Restart the game."
            return ()
    -- warn the player if health is critically low
    else if getHealth player < 11
        then do
            putStrLn mesg
            putStrLn "Critical Health level! Go eat something to get healthy again."
            play state'
    -- Check if the message indicates the player wants to quit
    else if mesg == "Quit"
        then do
            putStrLn "Exiting the program. Thank you for playing!"
            return ()
    else do
        putStrLn mesg -- Print the message from the command
        play state' -- Continue playing with the updated state

-- Main function to start the game
main :: IO ()
main = play state -- Call the play function with the initial game state
