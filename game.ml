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
let json_of_file fname = Yojson.Basic.from_file fname

(*[parse_effect j] parses JSON file [j] into a type effect record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
let parse_effect json =
  let gain = json |> member "gain" |> to_int in
  let loss = json |> member "loss" |> to_int in
  let house = json |> member "house_repair" |> to_int in
  let hotel = json |> member "hotel_repair" |> to_int in
  let multi = bool_of_string (json |> member "multiplier" |> to_string) in
  let location = json |> member "location" |> to_int in
  let delta = json |> member "move_x" |> to_int in
  let jail = bool_of_string (json |> member "jail" |> to_string) in
  {wealth_gain = gain; wealth_loss = loss; house_repair = house; hotel_repair = hotel;
   multiplier = multi; new_location = location; move_x = delta; get_out_of_jail = jail}

(*[parse_chance_cards j] parses JSON file [j] into a type chance card record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
let parse_chance_cards json =
  let card_id = json |> member "id" |> to_int in
  let card_name = json |> member "name" |> to_string in
  let card_description = json |> member "description" |> to_string in
  let card_effect = parse_effect (json |> member "effect") in
  {ch_id = card_id; ch_name = card_name; ch_description = card_description; ch_effect = card_effect}

(*[parse_community_cards j] parses JSON file [j] into a type community card record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
let parse_community_cards json =
  let card_id = json |> member "id" |> to_int in
  let card_name = json |> member "name" |> to_string in
  let card_description = json |> member "description" |> to_string in
  let card_effect = parse_effect (json |> member "effect") in
  {comm_id = card_id; comm_name = card_name; comm_description = card_description; comm_effect = card_effect}

(*[parse_tiles j] parses JSON file [j] into a type tile record.
 * requires: [j] contains all required field names with appropriate type.
 * Otherwise, raises an Yojson.Basic.Util.Type_error.*)
let parse_tiles json =
  let tile_id = json |> member "id" |> to_int in
  let tile_name = json |> member "name" |> to_string in
  let tile_type = json |> member "type" |> to_string in
  let tile_set = json |> member "set" |> to_int in
  let tile_cost = json |> member "cost" |> to_int in
  let tile_init_rent = json |> member "initial_rent" |> to_int in
  let tile_set_rent = json |> member "set_rent" |> to_int in
  let tile_rent_1 = json |> member "rent_1" |> to_int in
  let tile_rent_2 = json |> member "rent_2" |> to_int in
  let tile_rent_3 = json |> member "rent_3" |> to_int in
  let tile_rent_4 = json |> member "rent_4" |> to_int in
  let tile_rent_hotel = json |> member "rent_hotel" |> to_int in
  let tile_house = json |> member "house_cost" |> to_int in
  let tile_hotel = json |> member "hotel_cost" |> to_int in
  let tile_mortgage = json |> member "mortgage_cost" |> to_int in
  let tile_income_tax = bool_of_string (json |> member "income_tax" |> to_string) in
  let tile_effect = parse_effect (json |> member "effect") in

  {t_id=tile_id;
   name=tile_name;
   tiletype=tile_type;
   set=tile_set;
   cost=tile_cost;
   initial_rent=tile_init_rent;
   set_rent=tile_set_rent;
   rent_1_house=tile_rent_1;
   rent_2_house=tile_rent_2;
   rent_3_house=tile_rent_3;
   rent_4_house=tile_rent_4;
   rent_hotel=tile_rent_hotel;
   house_cost=tile_house;
   hotel_cost=tile_hotel;
   mortgage_cost=tile_mortgage;
   owner=100;
   curr_house=0;
   is_mortgaged=false;
   is_income_tax=tile_income_tax;
   effect = tile_effect}
