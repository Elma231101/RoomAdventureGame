module PingPong where

import World

pingPongName = "pingpong"
pingPong = MkItem { getUse = Just pingPongUse
               , getPut = True
               , getTake = True
               , getDisplay = "a pingPong table"
               }

pingPongUse :: State -> (State, String)
pingPongUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        --inventory' = [ i | i <- inventory, i /= pingPongName ]
        player' = MkPlayer { getInventory = inventory
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 30
                           , getScore = (getScore player) + 100
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...I just played ping pong...\n"
            ++ "\n And I won!!! yeeaahhh"
    in
        (state', mesg)
