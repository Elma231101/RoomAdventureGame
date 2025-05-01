module FastFood where

import World

fastFoodName = "fastfood"
fastFood = MkItem { getUse = Just fastFoodUse
               , getPut = True
               , getTake = True
               , getDisplay = "some fastfood"
               }

fastFoodUse :: State -> (State, String)
fastFoodUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= fastFoodName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) + 100
                           , getScore = (getScore player) + 5
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...That was delicious...\n"
            ++ "\n Didn't know I was that hungry!"
    in
        (state', mesg)
