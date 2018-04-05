open Str
open Game

(* [command] represents a command input by a player.
 * The commands are as follows
 * Roll - Rolls the dice for player to move
 * Buy - Allows user to buy the property if property does not have an owner
 * Sell - Allows user to sell one of his properties to another user or the bank
 * Trade - Allows user to trade properties/money with another user
 * Mortgage - Allows user to mortgage one of his properties
 * Inventory - Allows user to view all his properties and chance/community chest cards
 * UseChance - Allows user to use one of his chance cards in his inventory
 * UseCommunity - Allows user to use one os his community chest cards in his inventory
 * EndTurn - Allows the user to end his turn
 * Forfeit - Allows the user to quit the game *)
type command =
  | Turn_start
  | Buy_tile
  | Buy_mortgaged
  | Buy_houses of int
  | Buy_hotel
  | Sell_House of (int*int)
  | Sell_Hotel of int
  | Sell_Tile of int
  | Mortgage of int
  | Inventory
  | EndTurn
  | DoNothing
  | Quit

(* [parse str] is the command that represents player input [str].
 * requires: [str] is one of the commands forms described in the mli above *)
let parse str =
  let spaced = if (String.contains (String.lowercase_ascii str) ' ') then (String.lowercase_ascii str)
    else (String.lowercase_ascii str)^" " in
  let index = String.index spaced (' ') in
  let before_index = Str.string_before spaced index in
  let after_index = Str.string_after spaced (index+1) in
  if String.length after_index = 0 then
    if before_index = "quit" then Quit
    else if (before_index = "inv") || (before_index = "inventory") then Inventory
    else if before_index = "end" then EndTurn
    else DoNothing
  else
    if before_index = "buy" then Buy_tile
    else if before_index = "sell" then Sell_Tile (int_of_string after_index)
    else if before_index = "mortgage" then Mortgage (int_of_string after_index)
    else if before_index = "upgrade" then Buy_houses (int_of_string after_index)
    else DoNothing
