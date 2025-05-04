module Commands where


import Data.Char
import Data.Maybe
import World
import Game

-- Define the possible actions a player can take in the game
data Action =
    Move
  | Look
  | Put
  | Take
  | Use
  | Inventory
  | Quit
  | Help
  deriving (Eq, Show)

-- Define a type for objects, represented as a list of strings
type Object = [String]

-- Define a Command data structure that holds an action and an object
data Command =
  MkCommand { getAction::Maybe Action
            , getObject::Object
            }

-- Parse a string into an Action, returning Nothing if the action is unrecognized
parseAction :: String -> Maybe Action
parseAction actionString =
    case ( map toUpper actionString ) of
        "MOVE"      -> Just Move
        "LOOK"      -> Just Look
        "L"         -> Just Look
        "PUT"       -> Just Put
        "TAKE"      -> Just Take
        "USE"       -> Just Use
        "INVENTORY" -> Just Inventory
        "I"         -> Just Inventory
        "QUIT"      -> Just Quit
        "Q"         -> Just Quit
        "HELP"      -> Just Help
        "H"         -> Just Help
        _           -> Nothing

-- Help message that describes available commands and their effects
helpMesg :: String
helpMesg = "COMMAND          | EFFECT\n" ++
           "-----------------------------------------------------\n" ++
           "MOVE <DIRECTION> | Move in a given direction.\n" ++
           "L or LOOK        | Look at your surroundings.\n" ++
           "PUT <ITEM>       | Drop an item in your inventory.\n" ++
           "TAKE <ITEM>      | Take an item into your inventory.\n" ++
           "USE <ITEM>       | Use an item.\n" ++
           "I or INVENTORY   | Show your inventory.\n" ++
           "Q or QUIT        | Quit the game.\n" ++
           "H or HELP        | Show this help menu.\n"

-- Parse a raw input line into a Command
parseLine :: String -> Command
parseLine raw =
  MkCommand { getAction = parseAction . head $ words raw -- Get the action from the first word
            , getObject = tail $ words raw               -- Get the rest as the object
            }

-- Execute a command and return the new state and a message
parseCommand :: State -> Command -> (State, String)
parseCommand state command =
  case getAction command of
    Nothing   -> (state, "I don't know how to do that.")
    Just Move -> let
                   mDir = stringToDir $ head $ getObject command -- Convert the first object to a direction
                   state' = move state -- Move the player in the current state
                 in
                   case mDir of
                    Nothing -> (state, "I don't know where you want me to go.")
                    Just dir -> let state' = move state dir in (state', (look state')) -- Move and look around
    Just Look -> (state, look state) -- Look around the current location
    Just Take -> let
                   item = head $ getObject command -- Get the item to take
                   state' = Commands.take state item -- Attempt to take the item
                   ploc = getLocation $ getPlayer state -- Get the player's current location
                   ilocs = getItemLocations state  -- Get item locations in the state
                 in case elem item (fromJust $ lookup ploc ilocs) of
                   True -> (state', "Took the " ++ item ++ ".") -- Successfully took the item
                   False -> (state, "I don't see that.") -- Item not found
    Just Put -> let
                  item = head $ getObject command -- Get the item to put down
                  state' = Commands.put state item -- Attempt to put the item down
                  inventory = getInventory $ getPlayer state -- Get the player's inventory
                in case elem item inventory of
                  True -> (state', "Okay.") -- Successfully put down the item
                  False -> (state, "I don't have that.")
    Just Inventory -> (state, (showInventory state)) -- return the current state and the inventory display.
    Just Use -> let
                  item = head $ getObject command -- Get the item to use from the command
                  item' = fromJust $ lookup item items -- Look up the item in the items list to get its properties.
                  inventory = getInventory $ getPlayer state -- Get the player's current inventory.
                in
                  case elem item inventory of
                    False -> (state, "I'd have to have that first.") -- Cannot use item because we do not have it
                    True -> case getUse item' of
                              Nothing -> (state, "I don't know how to use that") -- There is no use function for the item
                              Just f -> f state -- apply the use function to the current state
    Just Quit -> (state, "Quit") -- send the quit message
    Just Help -> (state, helpMesg) -- send the help message
--    _ -> (state, "I'm not sure what you mean.")

-- Function to parse input from the user
parseInput :: State -> String -> (State, String)
parseInput state "" = (state, "") -- If the input string is empty, return the current state and an empty message.
parseInput state string = (parseCommand state) . parseLine $ string -- Otherwise, parse the command and return the new state and message.

-- Function to take an item from the game state
take :: State -> ItemName -> State
take state iname =
  let
    ploc = getLocation $ getPlayer state -- Get the player's current location.
    ilocs = getItemLocations state -- Get the item locations in the current state.
  in
    case getTake $ fromJust $ lookup iname items of -- Check if the item can be taken.
      False -> state -- return the current state unchanged.
      True -> case elem iname (fromJust $ lookup ploc ilocs) of  -- Check if the item is present in the player's location.
                  True -> takeItem state ploc iname -- call takeItem to update the state.
                  _     -> state -- return the current state unchanged.

-- Function to put an item down in the game state
put :: State -> ItemName -> State
put state iname =
  let
    ploc = getLocation $ getPlayer state -- Get the player's current location.
  in
    case getPut $ fromJust $ lookup iname items of -- Check if the item can be put down.
      False -> state -- return the current state unchanged.
      True -> case elem iname (getInventory $ getPlayer state) of -- Check if the item is in the player's inventory.
                False -> state
                True -> putItem state ploc iname -- call putItem to update the state.

-- Function to take an item from a room and add it to the player's inventory
takeItem :: State -> RoomName -> ItemName -> State
takeItem state rname item =
  let
    player = getPlayer state -- Get the current player.
    -- Check if the item is in the room; if so, create a list with the item, otherwise an empty list.
    item' = if elem item $ fromJust $ lookup rname $ getItemLocations state then [item] else []
    -- Create a new player with the updated inventory (adding the item).
    player' = MkPlayer { getInventory = (++) item' $ getInventory player
                       , getLocation = getLocation player
                       , getHealth = getHealth player
                       , getScore = getScore player
                       }
    -- Get the current list of items in the room.
    itemLocList = fromJust $ lookup rname $ getItemLocations state
    -- Create a new list of items in the room, excluding the item being taken.
    itemLocList' = [ i | i <- itemLocList, i /= item ]
    -- Create a pair of the room name and the updated item list.
    itemLocPair = (rname, itemLocList')
    -- Update the item locations in the state, replacing the old list with the new one.
    itemLoc = [ p | p <- getItemLocations state, p /= (rname, itemLocList) ] ++ [itemLocPair]
  in
    -- Return a new state with the updated item locations and player.
    MkState { getItemLocations = itemLoc
            , getPlayer = player'
            }

-- Function to put an item from the player's inventory into a room
putItem :: State -> RoomName -> ItemName -> State
putItem state rname item =
  let
    player = getPlayer state -- Get the current player.
    -- Create a new inventory excluding the item being put down.
    inventory = [ i | i <- getInventory player, i /= item ]
    -- Create a list with the item if it is in the inventory.
    item' = if elem item (getInventory player) then [item] else []
    -- Get the current list of items in the room.
    itemLocList = fromJust $ lookup rname $ getItemLocations state
    -- Create a new list of items in the room, including the item being put down.
    itemLocList' = itemLocList ++ item'
    -- Create a pair of the room name and the updated item list.
    itemLocPair = (rname, itemLocList')
    -- Update the item locations in the state, replacing the old list with the new one.
    itemLoc = [ p | p <- getItemLocations state, p /= (rname, itemLocList) ] ++ [itemLocPair]
    -- Create a new player with the updated inventory.
    player' = MkPlayer { getInventory = inventory
                       , getLocation = getLocation player
                       , getHealth = getHealth player
                       , getScore = getScore player
                       }
  in
    -- Return a new state with the updated item locations and player.
    MkState { getItemLocations = itemLoc
            , getPlayer = player'
            }

-- Function to move a player in a specified direction, returning a Maybe Player
move' :: Player -> Direction -> Maybe Player
move' player direction =
  do
    -- Look up the current room based on the player's location
    currentRoom <- lookup (getLocation player) rooms
    -- Look up the next location based on the direction provided
    nextLocation <- lookup direction $ getExits currentRoom
    -- Check if the next location is the attic
    if nextLocation == RoomName "Attic" then
      -- Check if the player has a chain in their inventory
      if "chain" `elem` getInventory player then
        -- If the player has a chain, return a new Player with the updated location
        return MkPlayer { getInventory = getInventory player
                         , getLocation = nextLocation
                         , getHealth = getHealth player
                         , getScore = getScore player
                         }
      else
        -- If the player does not have a chain, return Nothing
        Nothing
    else
      -- If the next location is not the attic, return a new Player with the updated location
      return MkPlayer { getInventory = getInventory player
                    , getLocation = nextLocation
                    , getHealth = getHealth player
                    , getScore = getScore player
                    }

-- Function to move the player within the game state
move :: State -> Direction -> State
move s d =
  let
    p = getPlayer s -- Get the current player from the state
    p' = move' p d -- Attempt to move the player in the specified direction
  in
    -- If successful, return a new state with the updated player; else return the current state unchanged
    case p' of
      Just p'' -> MkState { getPlayer = p'', getItemLocations = getItemLocations s}
      Nothing  -> s

-- Function to look around the current room and return a description
look :: State -> String
look state =
  let
    player = getPlayer state -- Get the current player from the state
    room = lookup (getLocation player) rooms -- Look up the current room based on the player's location
    desc = fromJust $ getDescription <$> room -- Get the room description (assuming it exists)
    exits = fromJust $ getExits <$> room -- Get the available exits from the room (assuming they exist)
    itemString = getItemDescriptions state -- Get a string describing items in the current location
    -- Check if the player's inventory contains a "chain"
    hasChain = "chain" `elem` getInventory player
    -- Remove "attic" from exits if the player does not have a chain
    updatedExits = if hasChain
                   then exits
                   else [ i | i <- exits, i /= (Up, RoomName "Attic") ]
  in
    -- Combine the room description, exits, and item descriptions into a single string
    parseDescription (desc, updatedExits) ++ itemString

-- Function to get a description of items in the current room
getItemDescriptions :: State -> String
getItemDescriptions state =
  let
    -- Look up the list of items in the player's current location
    lst = (lookup (getLocation $ getPlayer state) (getItemLocations state))
  in
    -- If there are items present, create a description string
    if (length $ fromJust lst) > 0 then
      "Here you see " ++ (listItems $ fromJust $ lst)
    else
      ""
-- Function to list items in a human-readable format
listItems :: [ItemName] -> String
listItems [] = "" -- If the list is empty, return an empty string.
listItems (i : []) = getDisplay $ fromJust $ lookup i items -- If there's one item, return its display representation.
listItems (i : (i': [])) = (getDisplay $ fromJust $ lookup i items) ++ ", and " ++ (getDisplay $ fromJust $ lookup i' items) -- If there are two items, format them with "and".
listItems (i : is) = (getDisplay $ fromJust $ lookup i items) ++ ", " ++ listItems is -- For more than two items, format the first item and recursively list the rest.

-- Function to parse a room description and its exits
parseDescription :: (Description, [(Direction, RoomName)]) -> String
parseDescription (Description desc, dirs) =
  let
    dirString = parseDescription' dirs -- Get the formatted string of exits.
  in
    desc ++ "\n" ++ (if dirString == "" then "There are no exits." else dirString) -- Combine the room description with the exits.

-- Helper function to format the exits of a room
parseDescription' :: [(Direction, RoomName)] -> String
parseDescription' [] = "" -- If there are no exits, return an empty string.
parseDescription' ((dir, roomName) : dirs) =
   -- Format the exit description and recursively process the rest.
  "There is an exit " ++ dirString ++ " leading to the " ++ roomString ++ ".\n" ++ parseDescription' dirs
  where dirString =
          case dir of
            North     -> "North"
            NorthEast -> "NorthEast"
            East      -> "East"
            SouthEast -> "SouthEast"
            South     -> "South"
            SouthWest -> "SouthWest"
            West      -> "West"
            NorthWest -> "NorthWest"
            Up        -> "Up"
            Down      -> "Down"
       
        roomString =
          case roomName of
            RoomName "Living Room" -> "Living Room"
            RoomName "Attic"       -> "Attic"
            RoomName "Garden"      -> "Garden"
            RoomName "Bed Room"    -> "Bed Room"
            RoomName "Basement"    -> "Basement"
            RoomName "Kitchen"     -> "Kitchen"
            RoomName "Hallway"     -> "Hallway"

-- Function to show the player's inventory along with health and score
showInventory :: State -> String
showInventory state = ((++) "You are carrying:\n" $ showInventory' $ getInventory $ getPlayer state)
                      ++ "\n\nYour health is: " ++ (show $ getHealth $ getPlayer state)
                      ++ "\nYour score is: " ++ (show $ getScore $ getPlayer state) ++ "\n"

-- Helper function to format the inventory items
showInventory' :: [ItemName] -> String
showInventory' [] = "" -- If the inventory is empty, return an empty string.
showInventory' (i : []) = getDisplay $ fromJust $ lookup i items -- If there's one item, return its display representation.
showInventory' (i : is) = (getDisplay $ fromJust $ lookup i items) ++ ",\n" ++ showInventory' is -- For multiple items, format the first item and recursively list the rest.
