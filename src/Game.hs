module Game where

import Rooms

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

pingPongName = "pingpong"
pingPong = MkItem { getUse = Just pingPongUse
               , getPut = True
               , getTake = True
               , getDisplay = "a pingPong table"
               }

pingPongUse :: State -> (State, String)
pingPongUse state =
    let
        score = getScore $ getPlayer state
        player = getPlayer state
        inventory = getInventory player
        --inventory' = [ i | i <- inventory, i /= pingPongName ]
        player' = MkPlayer { getInventory = inventory
                           , getLocation = getLocation player
                           , getHealth = (getHealth player) - 30
                           , getScore = (getScore player) + 100
                           }
        state' = MkState { getPlayer = player'
                         , getItemLocations = getItemLocations state
                         }
        mesg = "...I just played ping pong...\n"
            ++ "\n And I won!!! yeeaahhh"
    in
        (state', mesg)

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

denName = RoomName "Living Room"
atticName = RoomName "Attic"
gardenName = RoomName "Garden"
bedRoomName = RoomName "Bed Room"
basementName = RoomName "Basement"
kitchenName = RoomName "Kitchen"

livingRoom :: Room
livingRoom = MkRoom { getDescription = (Description "You are in the living-room. A wizard is snoring loudly on the couch.")
                    , getRoomName = denName
                    , getExits = [ (Up, atticName)
                                 , (West, gardenName)
                                 , (East, bedRoomName)
                                 , (South, kitchenName)]
                    }

bedRoom :: Room
bedRoom = MkRoom { getDescription = (Description "You are in the bedroom. Take a demetrin and get ready to sleep! zZzZz")
                    , getRoomName = bedRoomName
                    , getExits = [ (West, denName)
                                 , (Down, basementName)
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

basement :: Room
basement = MkRoom { getDescription = (Description "You are in the basement. There are tools for everything here.")
                    , getRoomName = basementName
                    , getExits = [ (Up, bedRoomName)
                                 ]
                  }

kitchen :: Room
kitchen = MkRoom { getDescription = (Description "You are in the kitchen. There is some fastfood in the counter.")
                    , getRoomName = kitchenName
                    , getExits = [ (North, denName)
                                 ]
                  }

rooms = [ (gardenName, gardenRoom)
        , (denName, livingRoom)
        , (atticName, atticRoom)
        , (bedRoomName, bedRoom)
        , (basementName, basement)
        , (kitchenName, kitchen)
        ]

player :: Player
player = MkPlayer { getInventory = []
                  , getLocation = denName
                  , getHealth = 100
                  , getScore = 0
                  }

state :: State
state = MkState { getItemLocations = [ (gardenName, [frogName, chainName])
                                     , (denName, [whiskeyName, bucketName])
                                     , (atticName, [weldingTorchName])
                                     , (bedRoomName, [demetrinName])
                                     , (basementName, [pingPongName, chainName])
                                     , (kitchenName, [fastFoodName])
                                     ]
                , getPlayer = player
                }