module Game where

import Rooms

bucketName = "bucket"
bucket = MkItem { getUse = Nothing
                , getPut = True
                , getTake = True
                , getDisplay = "a bucket"
                }

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

frogName = "frog"
frog = MkItem { getUse = Nothing
              , getPut = True
              , getTake = True
              , getDisplay = "a frog"
              }

chainName = "chain"
chain = MkItem { getUse = Nothing
               , getPut = True
               , getTake = True
               , getDisplay = "a chain"
               }

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
            ++ "\n Good night!! zzzz"
    in
        (state', mesg)

demetrinName = "demetrin"
demetrin = MkItem { getUse = Just demetrinUse
               , getPut = True
               , getTake = True
               , getDisplay = "a demetrin"
               }

items = [ (bucketName, bucket)
        , (whiskeyName, whiskey)
        , (frogName, frog)
        , (chainName, chain)
        , (emptyWhiskeyName, emptyWhiskey)
        , (demetrinName, demetrin)
        ]

denName = RoomName "Living Room"
atticName = RoomName "Attic"
gardenName = RoomName "Garden"
bedRoomName = RoomName "Bed Room"

livingRoom :: Room
livingRoom = MkRoom { getDescription = (Description "You are in the living-room. A wizard is snoring loudly on the couch.")
                    , getRoomName = denName
                    , getExits = [ (Up, atticName)
                                 , (West, gardenName)
                                 , (East, bedRoomName)]
                    }

bedRoom :: Room
bedRoom = MkRoom { getDescription = (Description "You are in the bed room. Take a demetrin and get ready to sleep! zZzZz")
                    , getRoomName = bedRoomName
                    , getExits = [ (West, denName)
                                 ]
                    }

atticRoom :: Room
atticRoom = MkRoom { getDescription = (Description "You are in the attic. There is a giant welding torch in the corner.")
                     , getRoomName = atticName
                     , getExits = [ (Down, denName)]
                     }

gardenRoom :: Room
gardenRoom = MkRoom { getDescription = (Description "You are in a beautiful garden. There is a well in front of you.")
                    , getRoomName = gardenName
                    , getExits = [(East, denName)]
                    }

rooms = [ (gardenName, gardenRoom)
        , (denName, livingRoom)
        , (atticName, atticRoom)
        , (bedRoomName, bedRoom)
        ]

player :: Player
player = MkPlayer { getInventory = []
                  , getLocation = bedRoomName
                  , getHealth = 100
                  , getScore = 0
                  }

state :: State
state = MkState { getItemLocations = [ (gardenName, [frogName, chainName])
                                     , (denName, [whiskeyName, bucketName])
                                     , (atticName, [])
                                     , (bedRoomName, [demetrinName])
                                     ]
                , getPlayer = player
                }