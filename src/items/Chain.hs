module Chain where

import World

chainUse :: State -> (State, String)
chainUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= chainName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 20
                           , getScore = (getScore player) + 100
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...I won the chain game in a fight...\n"
            ++ "\n It was a difficult game, but I won.... "
    in
        (state', mesg)

chainName = "chain"
chain = MkItem { getUse = Just chainUse
               , getPut = True
               , getTake = True
               , getDisplay = "a chain"
               }
