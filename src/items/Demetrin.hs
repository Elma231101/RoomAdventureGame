module Demetrin where

import World

demetrinUse :: State -> (State, String)
demetrinUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= demetrinName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) + 2
                           , getScore = (getScore player) + 50
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...Now I am ready to sleep...\n"
            ++ "\n Good night!! zZzZz"
    in
        (state', mesg)

demetrinName = "demetrin"
demetrin = MkItem { getUse = Just demetrinUse
               , getPut = True
               , getTake = True
               , getDisplay = "a demetrin"
               }
