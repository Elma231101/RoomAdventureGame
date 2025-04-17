# RoomAdventureGame - RAG

RAG is a text-adventure game platform written in the Haskell programming
language. This was done as a learning exercise to better understand Haskell.

## Building the project
In order to use RAG, you need to have installed `stack`, instructions for
installation can be found
[here](https://docs.haskellstack.org/en/stable/README/#how-to-install).

Once you have stack installed, and have cloned the repository for RAG,
run either `stack run` from within the repository,
depending on your stack installation.

## Tutorial
### Playing RAG
In order to play RAG, there are 6 commands you should understand.

- `use <item>`: This allows you to use an item. If the item in question does not
have a defined use, the game will inform you that you do not know how to use the
item.
- `move <direction>`: This allows you to move in any of the 8 directions on a
compass, as well as upward or downward.
- `look`: This allows you to look at your surroundings, and will output a text
description of your current location into your terminal. This is done automatically
any time you move to a new location.
- `take <item>`: This allows you to pick up an item in the same location as you
and place it in your inventory.
- `put <item>`: This allows you to place an item from your inventory into your
current location
- `help`: This will display a help menu reiterating what you have just read.
