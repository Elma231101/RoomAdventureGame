module Bucket where

import World

bucketUse :: State -> (State, String)
bucketUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        inventory' = [ i | i <- inventory, i /= bucketName ]
        player' = MkPlayer { getInventory = inventory'
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 15
                           , getScore = (getScore player) + 50
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...I filled 10 buckets with water....\n"
            ++ "\n This is difficult but also interesting..."
    in
        (state', mesg)

bucketName = "bucket"
bucket = MkItem { getUse = Just bucketUse
                , getPut = True
                , getTake = True
                , getDisplay = "a bucket"
                }
