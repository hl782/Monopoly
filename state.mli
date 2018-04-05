(*[Invalid_Command] is an exception that is raised whenever an invalid command is detected. *)
exception Invalid_Command of string
open Command
open Game

(*[player] defines the state of a player*)
type player = {
  name: string;
  id: int;
  money: int;
  current_location: int;
  owned_property: tile list;
  mortgaged_property: tile list;
  jail_count: int;
  jail_card : bool;
  card_drawn : bool;
  community_drawn : Game.community_card;
  chance_drawn : Game.chance_card;
}

(*[game_state] is an abstract type representing the state of a turn of monopoly game.
  It contains the states of all players and the states of all tiles in the monopoly game.*)
type game_state = {
  turn_count: int;
  current_turn: int;
  winner: player;
  free_parking: int;
  chance_deck: chance_card list;
  community_deck: community_card list;
  active_players: player list;
  board : tile list;
  tax_collected : int;
  double : bool; (*this is for checking if a double is rolled. Our idea is to
                   keep continue roll dice and do exec if game_state.double = True.
                   the game_state.double will change in roll_dice function.*)
  dices : int*int;
  game_finished : bool;
}

(*[empty_player] is player with empty fields.*)
val empty_player : player

(*[empty_state] is state with empty fields.*)
val empty_state : game_state

(*[empty_community_card] is an empty community card.*)
val empty_community_card: Game.community_card

(*[empty_chance_card] is an empty chance card.*)
val empty_chance_card: Game.chance_card

(*[player_state s i] is a player’s state whose index is [i] in players list of [s] .*)
val get_player_state : game_state -> int -> player

(*[tile_state s i] is a tile’s state whose index is [i] in tiles list of [s].*)
val get_tile_state : game_state -> int -> Game.tile

(*[owned_property p] returns list of property that player p owns*)
val owned_property : player -> tile list

(*[name p] returns name of player p*)
val name : player -> string

(*[location p] returns location of player p*)
val location : player -> int

(*[jail_count p] returns the turns in jail left for player p*)
val jail_count : player -> int

(*[money p] returns amount of money player p has*)
val money : player -> int

(*[active_players gs] returns list of active players in game*)
val active_players : game_state -> player list

(*[current_turn s] is an index that represents the player with current turn in players
  list of [s].*)
val current_turn : game_state -> int

(*[curr_turn_id s] is the id of the player with current turn in players
  list of [s].*)
val curr_turn_id : game_state -> int

(* [curr_tile st] is a current tile of the current player *)
val curr_tile : game_state -> tile

(* [curr_turn_player st] is a player state of the current player *)
val curr_turn_player : game_state -> player

(*[fst_dice st] is the first dice rolled in [st]*)
val fst_dice : game_state -> int

(*[snd_dice st] is the second dice rolled in [st]*)
val snd_dice : game_state -> int

(*[game_fin st] is true if there is a winner. Otherwise, false.*)
val game_fin : game_state -> bool

(*[check_winner st] is true if there is only one active player. Otherwise, false.*)
val check_winner : game_state -> bool

(*[id_to_index i st] returns the index of player with id i in game_state st *)
val id_to_index : int -> game_state -> int

(*[get_last_chance_drawn p] returns the last chance card drawn for [p] *)
val get_last_chance_drawn : player -> Game.chance_card

(*[get_last_comm_drawn p] returns the last community card drawn for [p] *)
val get_last_comm_drawn : player -> Game.community_card

(*[get_tax_collected st] returns the collected tax in [st] *)
val get_tax_collected : game_state -> int

(*[roll_dice i] is a new state with the location of the player whose index is [i]
  is updated depending on the result of rolling two dice. If the player is in jail
  and has been in three consecutive turns, the player is allowed to roll the dice
  and move to the location. If the player is in jail and has been in less than three
  consecutive turns and rolls a double, the player is allowed to move. If the player
  fails to roll a double, the number of turns the player is in jail increases but
  fails to move to a new location.*)
val roll_dice : game_state -> game_state

(*[init_state b chest chance p] is an initial state of monopoly game in which the tiles of the board
  game are defined in [b]. The number of players will be determined by [p]. The community chest cards are
  determined by [chest] and the chance cards are determined by [chance]
  requires:  [b] represents an error-free monopoly board.*)
val init_state : Yojson.Basic.json -> Yojson.Basic.json -> Yojson.Basic.json -> int -> game_state

(*[exec c st] is a new state that is resulted after executing command [c].
  Depending on a type of the tile that the current-turn player is located at in [st],
  a set of allowed commands changes. Command [quit] and [inventory] and [end_turn]
  are always allowed, and, for those commands, [exec c st] is [st].
  If the player goes bankrupt (does not have sufficient money to pay rent or tax),
  the player will be taken out from the game. Otherwise, if the type of the tile is...
  1. Go: No other command is allowed. The player collects $200.
  2. Properties / Utilities / Train Station:
    - If the tile belongs to another player, subtract the player’s wealth by the
      amount of rent that the player needs to pay. In such case, possible commands are
      Possible commands:
        - Sell: The player has to sell one or more of his properties, utilities, or train station.
        - Mortgage: Unimproved properties can be mortgaged through the bank.
        - Use Chance: The player can use the chance card to exempt from the payment.
        - Use Community: The player can use the community chest to exempt from the payment
        - Forfeit : Surrender from the game!
    - If the tile belongs to the player,
        - Upgrade Property (only for properties): The player can upgrade his property by purchasing houses or hotels.
        - End: The player can end his turn after 0 or more commands.
        - Forfeit : Surrender from the game!
    - If the tile doesn’t belong to any player
        - Buy : If the player has enough money to buy the land, he can buy the unimproved land.
        - Upgrade Property : The player can upgrade his property by purchasing houses or hotels
        - End: The player can end his turn after 0 or more commands.
        - Forfeit : Surrender from the game!
  3. Community Chest: The player collects a community chest card. If the community
     chest card can be used later, the player is allowed to keep it for later used.
     Otherwise, [exec c st] is a new state produced following the direction of community chest card.
  4. Chance: The player collects a chance card. If the chance card can be used later,
     the player is allowed to keep it for later used. Otherwise, [exec c st] is
     a new state produced following the direction of chance card.
  5. Income Tax: The player pays 10% of the owned properties as tax.
    Possible commands:
      - Sell: The player has to sell one or more of his properties, utilities, or train station.
      - Mortgage: Unimproved properties can be mortgaged through the bank.
      - Forfeit : Surrender from the game!
  6. Jail: [exec c st] is [st]. The jail component of monopoly is handled in [roll_dice].
    Possible commands:
      - Use Chance : The player can use “Get Out of Jail” card to escape from jail.
      - Use Community : The player can use “Get Out of Jail” card to escape from jail.
      - Forfeit : Surrender from the game!
  7. Free Parking: No other command is allowed. [exec c st] is [st].
  8. Super Tax: No other command is allowed. The player pays $100 as tax.
    Possible commands:
      - Sell: The player has to sell one or more of his properties, utilities, or train station.
      - Mortgage: Unimproved properties can be mortgaged through the bank.
      - Forfeit : Surrender from the game!
  9. Go to Jail: No other command is allowed. The player goes to jail.
    Possible commands:
      - Use Chance : The player can use “Get Out of Jail” card to escape from jail.
      - Use Community : The player can use “Get Out of Jail” card to escape from jail.
      - Forfeit : Surrender from the game!
  10. effects: [exec] raises an exception [Invalid_Command] if [c] is not an
      allowed command. The raised exception is guaranteed to be handled in main.ml.
      Otherwise, [exec c st] will not have any effects on the output of the program.
  11. requires: [st] is either an error-free state produced by [init_state j p] or
      an error-free state produced by [exec c st]. *)
val exec : Command.command -> game_state -> game_state
