module Whiskey where

import World

whiskeyUse :: State -> (State, String)
whiskeyUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= whiskeyName ] ++ [ emptyWhiskeyName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 15
                           , getScore = (getScore player) + 50
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...GLUG GLUG GLUG GLUG...\n"
            ++ "\n Aaah... Oddly refreshing."
    in
        (state', mesg)

whiskeyName = "whiskey"
whiskey = MkItem { getUse = Just whiskeyUse
                 , getPut = True
                 , getTake = True
                 , getDisplay = "a bottle of whiskey"
                 }

emptyWhiskeyName = "bottle"
emptyWhiskey = MkItem { getUse = Nothing
                      , getPut = True
                      , getTake = True
                      , getDisplay = "an empty bottle with a label reading 'WHISKEY'"
                      }
