module Game where

import World
import AllItems
import AllRooms
import RoomNames

items = [ (bucketName, bucket)
        , (whiskeyName, whiskey)
        , (frogName, frog)
        , (chainName, chain)
        , (emptyWhiskeyName, emptyWhiskey)
        , (demetrinName, demetrin)
        , (pingPongName, pingPong)
        , (weldingTorchName, weldingTorch)
        , (fastFoodName, fastFood)
        ]

rooms = [ (gardenName, gardenRoom)
        , (livingRoomName, livingRoom)
        , (atticName, atticRoom)
        , (bedRoomName, bedRoom)
        , (basementName, basement)
        , (kitchenName, kitchen)
        , (hallwayName, hallway)
        ]

player :: Player
player = MkPlayer { getInventory = []
                  , getLocation = livingRoomName
                  , getHealth = 100
                  , getScore = 0
                  }

state :: State
state = MkState { getItemLocations = [ (gardenName, [frogName, chainName, bucketName])
                                     , (livingRoomName, [whiskeyName])
                                     , (atticName, [weldingTorchName])
                                     , (bedRoomName, [demetrinName])
                                     , (basementName, [pingPongName, chainName])
                                     , (kitchenName, [fastFoodName])
                                     , (hallwayName, [])
                                     ]
                , getPlayer = player
                }
