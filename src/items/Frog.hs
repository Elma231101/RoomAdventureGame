module Frog where

import World

frogUse :: State -> (State, String)
frogUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= frogName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) + 60
                           , getScore = (getScore player) + 5
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...The frog won the race...\n"
            ++ "\n Yeahh, Applauded... "
    in
        (state', mesg)

frogName = "frog"
frog = MkItem { getUse = Just frogUse
              , getPut = True
              , getTake = True
              , getDisplay = "a frog"
              }
