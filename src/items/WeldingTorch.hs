module WeldingTorch where

import World

weldingTorchName = "weldingtorch"
weldingTorch = MkItem { getUse = Just weldingTorchUse
               , getPut = True
               , getTake = True
               , getDisplay = "a welding torch"
               }

weldingTorchUse :: State -> (State, String)
weldingTorchUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= weldingTorchName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 10
                           , getScore = (getScore player) + 50
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...I am done with the welding now...\n"
            ++ "\n Happy with how that turned out!!"
    in
        (state', mesg)
