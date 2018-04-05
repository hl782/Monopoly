open Yojson.Basic.Util

(*[effect] defines the after effects of drawing a chance or community chest card*)
type effect = {
  wealth_gain: int;
  wealth_loss: int;
  house_repair: int;
  hotel_repair: int;
  multiplier: bool;
  new_location: int;
  move_x: int;
  get_out_of_jail: bool;
}

(*[tile] defines the information of a tile on the board*)
type tile = {
  t_id: int;
  name: string;
  tiletype: string;
  set: int;
  cost: int;
  initial_rent: int;
  set_rent: int;
  rent_1_house: int;
  rent_2_house: int;
  rent_3_house: int;
  rent_4_house: int;
  rent_hotel: int;
  house_cost: int;
  hotel_cost: int;
  mortgage_cost: int;
  owner: int;
  curr_house: int;
  is_mortgaged: bool;
  is_income_tax: bool;
  effect: effect;
}

(*[board] defines the representation of the board. It is a list of tiles *)
type board = tile list

(*[community_card] defines a community chest card *)
type community_card  = {
  comm_id: int;
  comm_name: string;
  comm_description: string;
  comm_effect: effect;
}

(*[chance_card] defines a chance card *)
type chance_card = {
  ch_id: int;
  ch_name: string;
  ch_description: string;
  ch_effect: effect;
}

(*[json_of_file s] is JSON file with file name [s] *)
val json_of_file : string -> Yojson.Basic.json

(*[parse_effect j] parses JSON file [j] into a type effect record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
val parse_effect : Yojson.Basic.json -> effect

(*[parse_chance_cards j] parses JSON file [j] into a type chance card record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
val parse_chance_cards : Yojson.Basic.json -> chance_card

(*[parse_community_cards j] parses JSON file [j] into a type community card record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
val parse_community_cards : Yojson.Basic.json -> community_card

(*[parse_tiles j] parses JSON file [j] into a type tile record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
val parse_tiles : Yojson.Basic.json -> tile
