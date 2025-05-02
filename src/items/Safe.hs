module Safe where

import World

safeUse :: State -> (State, String)
safeUse state =
    let
        player = getPlayer state
        inventory = getInventory player
        
        -- Check if the key is in the inventory
        hasKey = "key" `elem` inventory
        
        -- If the player has the key, proceed to open the safe
        (state', mesg) = if hasKey
            then
                let
                    score = getScore player
                    inventory' = [ i | i <- inventory, i /= safeName ]
                    player' = MkPlayer { getInventory = inventory'
                                       , getLocation = getLocation player
                                       , getHealth = getHealth player
                                       , getScore = score + 150
                                       }
                    state' = MkState { getPlayer = player'
                                     , getItemLocations = getItemLocations state
                                     }
                    mesg = "...You found the safe and opened it...\n"
                        ++ "You have solved the puzzle!"
                in
                    (state', mesg)
            else
                -- If the player does not have the key
                (state, "You need a key to open the safe.")
    in
        (state', mesg)

safeName = "safe"
safe = MkItem { getUse = Just safeUse
              , getPut = True
              , getTake = True
              , getDisplay = "a safe"
              }











