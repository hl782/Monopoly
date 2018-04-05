
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
val parse : string -> command
