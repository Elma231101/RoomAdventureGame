module Commands where


import Data.Char
import Data.Maybe
import Rooms
import Game

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

type Object = [String]

data Command =
  MkCommand { getAction::Maybe Action
            , getObject::Object
            }

parseAction :: String -> Maybe Action
parseAction actionString =
    case ( map toUpper actionString ) of
        "MOVE"      -> Just Move
        "LOOK"      -> Just Look
        "PUT"       -> Just Put
        "TAKE"      -> Just Take
        "USE"       -> Just Use
        "INVENTORY" -> Just Inventory
        "HELP"      -> Just Help
        _           -> Nothing

helpMesg :: String
helpMesg = "AVAILABLE COMMANDS:" ++
           "   COMMAND               EFFECT  \n" ++
           "MOVE <DIRECTION> -> Move in a given direction.\n" ++
           "LOOK             -> Look at your surroundings.\n" ++
           "PUT <ITEM>       -> Drop an item in your inventory.\n" ++
           "TAKE <ITEM>      -> Take an item into your inventory.\n" ++
           "USE <ITEM>       -> Use an item.\n" ++
           "INVENTORY        -> Show your inventory.\n" ++
           "HELP             -> Show this help menu.\n"

parseLine :: String -> Command
parseLine raw =
  MkCommand { getAction = parseAction . head $ words raw
            , getObject = tail $ words raw
            }

parseCommand :: State -> Command -> (State, String)
parseCommand state command =
  case getAction command of
    Nothing   -> (state, "I don't know how to do that.")
    Just Move -> let
                   mDir = stringToDir $ head $ getObject command
                   state' = move state 
                 in
                   case mDir of
                    Nothing -> (state, "I don't know where you want me to go.")
                    Just dir -> let state' = move state dir in (state', (look state'))
    Just Look -> (state, look state)
    Just Take -> let
                   item = head $ getObject command
                   state' = Commands.take state item
                   ploc = getLocation $ getPlayer state
                   ilocs = getItemLocations state
                 in case elem item (fromJust $ lookup ploc ilocs) of
                   True -> (state', "Took the " ++ item ++ ".")
                   False -> (state, "I don't see that.")
    Just Put -> let
                  item = head $ getObject command
                  state' = Commands.put state item
                  inventory = getInventory $ getPlayer state
                in case elem item inventory of
                  True -> (state', "Okay.")
                  False -> (state, "I don't have that.")
    Just Inventory -> (state, (showInventory state))
    Just Use -> let
                  item = head $ getObject command
                  item' = fromJust $ lookup item items
                  inventory = getInventory $ getPlayer state
                in
                  case elem item inventory of
                    False -> (state, "I'd have to have that first.")
                    True -> case getUse item' of
                              Nothing -> (state, "I don't know how to use that")
                              Just f -> f state
    Just Help -> (state, helpMesg)
    _ -> (state, "I'm not sure what you mean.")

parseInput :: State -> String -> (State, String)
parseInput state "" = (state, "")
parseInput state string = (parseCommand state) . parseLine $ string


take :: State -> ItemName -> State
take state iname =
  let
    ploc = getLocation $ getPlayer state
    ilocs = getItemLocations state
  in
    case getTake $ fromJust $ lookup iname items of
      False -> state
      True -> case elem iname (fromJust $ lookup ploc ilocs) of
                  True -> takeItem state ploc iname
                  _     -> state

put :: State -> ItemName -> State
put state iname =
  let
    ploc = getLocation $ getPlayer state
  in
    case getPut $ fromJust $ lookup iname items of
      False -> state
      True -> case elem iname (getInventory $ getPlayer state) of
                False -> state
                True -> putItem state ploc iname
