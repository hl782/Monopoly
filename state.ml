open Game
open Command
open Yojson.Basic.Util
open Random

exception Invalid_Command of string
exception Bankrupt

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
  double : bool;
  dices : int*int;
  game_finished : bool;
}

let (empty_effect : Game.effect) = {
  wealth_gain= 0;
  wealth_loss= 0;
  house_repair= 0;
  hotel_repair= 0;
  multiplier= false;
  new_location= 0;
  move_x= 0;
  get_out_of_jail= false;
}

let (empty_community_card :Game.community_card) =
  {comm_id=100000; comm_name=""; comm_description = ""; comm_effect = empty_effect}

let (empty_chance_card :Game.chance_card) =
  {ch_id=100000; ch_name=""; ch_description = ""; ch_effect = empty_effect}

let empty_player =
  { name= "";
    id= 0;
    money= 0;
    current_location= 0;
    owned_property= [];
    mortgaged_property= [];
    jail_count= 0;
    jail_card = false;
    card_drawn = false;
    community_drawn = empty_community_card;
    chance_drawn = empty_chance_card}

let empty_state =
  { turn_count= 0;
    current_turn= 0;
    winner= empty_player;
    free_parking= 0;
    chance_deck= [];
    community_deck= [];
    active_players= [];
    board = [];
    tax_collected = 0;
    double = false;
    dices = (0,0);
    game_finished = false;
  }

let init_player b player_num =
  let player_name = "player" ^ string_of_int (player_num) in
  {name = player_name; id = player_num; money = 1000; current_location = 0;
   owned_property = []; jail_count = 0; jail_card = false;
   card_drawn = false; community_drawn = empty_community_card;
   chance_drawn = empty_chance_card; mortgaged_property = []}

let rec init_player_list b player_num acc =
  if player_num = 0 then acc
  else
    init_player_list b (player_num-1) ((init_player b player_num)::acc)

(*[init_state b chest chance p] is an initial state of monopoly game in which the
 * tiles of the board game are defined in [b]. The number of players will be determined
 * by [p]. The community chest cards are determined by [chest] and the chance cards
 * are determined by [chance].
 * requires:  [b] represents an error-free monopoly board.*)
let init_state tile chance community num_players =
  let chance_cards = List.map (Game.parse_chance_cards) (chance |> member "cards" |> to_list) in
  let community_cards = List.map (Game.parse_community_cards) (community |> member "cards" |> to_list) in
  let tiles = List.map (Game.parse_tiles) (tile |> member "tiles" |> to_list) in
  let player_list = init_player_list tiles num_players [] in

  {turn_count=0;
   current_turn= 0;
   winner=empty_player;
   free_parking=0;
   chance_deck=chance_cards;
   tax_collected = 0;
   community_deck = community_cards;
   active_players=player_list;
   board = tiles;
   double = false;
   dices = (0,0);
   game_finished = false;}

(*[player_state s i] is a player’s state whose index is [i] in players list of [s] .*)
let get_player_state s i =
  List.nth s.active_players i

(*[tile_state s i] is a tile’s state whose index is [i] in tiles list of [s].*)
let get_tile_state s i =
  List.nth s.board i

(*[owned_property p] returns list of property that player p owns*)
let owned_property p =
  p.owned_property

(*[name p] returns name of player p*)
let name p =
  p.name

(*[money p] returns amount of money player p has*)
let money p =
  p.money

(*[location p] returns current location of player p*)
let location p =
  p.current_location

(*[jail_count p] returns turns in jail left for player p*)
let jail_count p =
  p.jail_count

(*[active_players gs] returns list of active players in game*)
let active_players gs =
  gs.active_players

(*[turns gs] is the number of turns including current state*)
let turns gs =
  gs.turn_count

(*[current_turn gs] returns the player with current turn*)
let current_turn gs =
  gs.current_turn

(*[winner gs] is the winner of the game. If there is no winner, return exception.*)
let winner gs =
  gs.winner

(* [curr_turn_id st] returns the id of current player *)
let curr_turn_id st =
  (List.nth st.active_players st.current_turn).id

(* [curr_turn_id st] returns the current player *)
let curr_turn_player st =
  (List.nth st.active_players st.current_turn)

(* [curr_tile st] is a current tile of the current player *)
let curr_tile st =
  List.nth st.board (curr_turn_player st).current_location

(*[fst_dice st] is the first dice rolled in [st]*)
let fst_dice st =
  let fst (x,_) = x in
  fst (st.dices)

(*[snd_dice st] is the second dice rolled in [st]*)
let snd_dice st =
  let snd (_,x) = x in
  snd (st.dices)

(*[check_winner st] is true if there is only one active player. Otherwise, false.*)
let check_winner st =
  (List.length st.active_players = 1)

(*[game_fin st] is true if there is a winner. Otherwise, false.*)
let game_fin st =
  st.game_finished

(*[get_last_chance_drawn p] returns the last chance card drawn for [p] *)
let get_last_chance_drawn p =
  p.chance_drawn

(*[get_last_comm_drawn p] returns the last community card drawn for [p] *)
let get_last_comm_drawn p =
  p.community_drawn

(*[get_tax_collected st] returns the collected tax in [st] *)
let get_tax_collected st =
  st.tax_collected

(*[id_to_index_h i lst 0] returns the index of player with id i in the
 * player list lst *)
let rec id_to_index_h i lst acc =
  match lst with
  | [] -> failwith "player id not in active player list"
  | h::t -> if h.id = i then acc else (id_to_index_h i t (acc+1))

(*[id_to_index i st] returns the index of player with id i in game_state st *)
let id_to_index i st = id_to_index_h i st.active_players 0

(*############################EXEC PART########################################*)
(*-------------HELPERS---------------*)
(* [find_index elt lst acc] is the helper function used to find the index of
 * player in the active player list*)
let rec find_index elt lst acc =
  match lst with
  | [] -> failwith "Not found"
  | h::t -> if h = elt then acc else find_index elt t (acc+1)

(*[change_player_money st m] is a new active player list with current player's
 * money altered by [m].*)
let change_player_money st m =
  List.map
    (fun x ->
       if x.id = (curr_turn_id st) then
         {x with money = x.money + m}
       else x) st.active_players

(*[change_player_money_for_rent st m owner_id] is a new active player list with
  current player's money and owner's money altered by [m] .*)
let change_player_money_for_rent st m owner_id =
  List.map
    (fun x ->
       if x.id = (curr_turn_id st) then {x with money = x.money - m}
       else if x.id = owner_id then {x with money = x.money + m}
       else x) st.active_players

(*[change_money_owned_prop st m t] is a new active player list with current player's
 * money altered by [m] and owned property by adding [t].*)
let change_money_owned_prop st m t =
  List.map
    (fun x ->
       if x.id = (curr_turn_id st) then
         {x with money = x.money + m; owned_property = t::x.owned_property}
       else x) st.active_players

(*[change_m_owned_mort st m t] is a new active player list with current player's
 * money altered by [m] and owned property by adding [t] and removing [t]
 * from mortgaged_property.*)
let change_m_owned_mort st m t =
  let mort_removed =
    List.filter (fun x -> x <> t) (curr_turn_player st).mortgaged_property in

  List.map
    (fun x ->
       if x.id = (curr_turn_id st) then
         {x with money = x.money + m;
                 owned_property = t::x.owned_property;
                 mortgaged_property = mort_removed}
       else x) st.active_players

(*[change_m_mort_for_sell] is a new active player list with current player's
 * money altered by [m] and owned property by removing [t].*)
let change_m_owned_for_sell st m t =
  let owned_removed =
    List.filter (fun x -> x.t_id <> t.t_id) (curr_turn_player st).owned_property in

  List.map
    (fun x -> if x.id = (curr_turn_id st) then
        {x with money = x.money + m; owned_property = owned_removed}
      else x) st.active_players

(*[change_m_mort_for_sell] is a new active player list with current player's
 * money altered by [m] and  removing [t] from mortgaged_property.*)
let change_m_mort_for_sell st m t =
  let mort_removed =
    List.filter (fun x -> x.t_id <> t.t_id) (curr_turn_player st).mortgaged_property in
  List.map
    (fun x ->
       if x.id = (curr_turn_id st) then
         {x with money = x.money + m; mortgaged_property = mort_removed}
       else x) st.active_players

(* HELPERS FOR BUY COMMAND*)
(*[buy_tile st] is a state where the player buys current tile. It is guaranteed
 * that current tile does not belong to anyone. If the player does not have enough
 * money to buy, then raise Invalid Command.*)
let buy_tile st =
  if (curr_turn_player st).money >= (curr_tile st).cost then
    begin
      let new_board = List.map (fun x ->
            if x = (curr_tile st) then {x with owner = (curr_turn_id st)}
            else x) st.board in
      let new_active_players =
        change_money_owned_prop st (-((curr_tile st).cost)) (curr_tile st) in

      {st with active_players = new_active_players;
               board = new_board}
    end

  else
      raise (Invalid_Command "You do not have enough money to buy a tile")

(*[buy_mortgaged st] is a state where the player buys a mortgaged tile. It is guaranteed
 * that current tile does belong to the player. If the player does not have enough
 * money to buy, then raise Invalid Command.*)
let buy_mortgaged st =
  if (curr_turn_player st).money >= (curr_tile st).mortgage_cost then
    begin
      let new_board = List.map (fun x -> if x = (curr_tile st) then
                                            {x with is_mortgaged = false;}
                                          else x) st.board in
      let new_active_players =
        change_m_owned_mort st (-((curr_tile st).mortgage_cost)) (curr_tile st)in
      {st with active_players = new_active_players; board = new_board}
    end

  else
    raise (Invalid_Command "You do not have enough money to buy a tile")

(*[buy_houses st n] is a state where the player buys house(s) on current tile. Guaranteed
 * that current tile does belong to the current player. It is guaranteed that
 * if the player tries to build a building, then he owns every set tiles. If the
 * player does not have enough money to buy, then raise Invalid Command.
 * Precondition: n + # of curr_house < 5 *)
let buy_houses st n =
  let condition_of_n = n + (curr_tile st).curr_house < 5 in
  if ((curr_turn_player st).money >= (curr_tile st).house_cost * n) && condition_of_n then
    begin
      let new_board = List.map (fun x -> if x = (curr_tile st) then
                                           {x with curr_house = x.curr_house + n}
                                         else x) st.board in
      let new_active_players =
        change_player_money st ((-((curr_tile st).house_cost))*n) in
      {st with active_players = new_active_players;
               board = new_board}
    end

  else
    raise (Invalid_Command "You do not have enough money to buy houses")

(*[buy_hotel st] is a state where the player buys a hotel on current tile. It is guaranteed
 * that current tile does belong to the current player. It is guaranteed that
 * if the player tries to build a hotel, then he owns 4 houses on this tile. If the
 * player does not have enough money to buy, then raise Invalid Command.*)
let buy_hotel st =
  if (curr_turn_player st).money >= (curr_tile st).hotel_cost then
    begin
      let new_board = List.map (fun x -> if x = (curr_tile st) then
                                            {x with curr_house = 5}
                                         else x) st.board in
      let new_active_players =
        change_player_money st ((-((curr_tile st).hotel_cost))) in
      {st with active_players = new_active_players;
               board = new_board}
    end

  else
    raise (Invalid_Command "You do not have enough money to buy houses")

(* HELPERS FOR SELL COMMAND*)
(*[sell_houses st n] is a state where the player sells house(s) on current tile.
 * The current tile must belong to the current player. If the
 * player does not have enough building to sell, then raise Invalid Command. *)
let sell_houses st n index =
  let tile_to_sell = List.nth st.board index in
  let condition = (tile_to_sell.owner = (curr_turn_id st)) in
  let curr_house = tile_to_sell.curr_house in
  if (curr_house < 5) && (n > 0) && (curr_house - n >= 0) && condition then
    begin
      let new_board =
        List.map (fun x -> if x.t_id = tile_to_sell.t_id then
                              {x with curr_house = x.curr_house - n}
                           else x) st.board in
      let new_active_players =
        change_player_money st ((tile_to_sell.house_cost*n)/2) in

      {st with active_players = new_active_players;
               board = new_board}
    end

  else
    raise (Invalid_Command "You cannot sell these houses")

(*[sell_hotel] is a state where the player sells hotel on current tile.
 * The tile must belong to the current player. If the
 * player does not have enough building to sell, then raise Invalid Command. *)
let sell_hotel st index =
  let tile_to_sell = List.nth st.board index in
  let condition = (tile_to_sell.owner = (curr_turn_id st)) in
  if tile_to_sell.curr_house = 5 && condition then
    begin
      let new_board =
        List.map (fun x -> if x.t_id = tile_to_sell.t_id then
                              {x with curr_house = 0}
                           else x) st.board in
      let new_active_players =
        change_player_money st ((tile_to_sell.hotel_cost + (4*tile_to_sell.house_cost))/2) in
      {st with active_players = new_active_players;
               board = new_board}
    end

  else
    raise (Invalid_Command "You cannot sell this hotel")

(*[sell_hotel] is a state where the player sells the tile with [index].
 * The tile must belong to the current player. If the
 * condition is not met, then raise Invalid Command. *)
let sell_tile st index =
  let tile_to_sell = List.nth st.board index in
  let condition = (tile_to_sell.owner = (curr_turn_id st)) in
  let curr_house = tile_to_sell.curr_house in
  (*When trying to sell a tile that is not mortgaged.*)
  if curr_house = 0 && condition && not tile_to_sell.is_mortgaged then
    begin
      let new_board = List.map (fun x -> if x.t_id = tile_to_sell.t_id then
                                            {x with owner = 100}
                                         else x) st.board in
      let new_active_players =
        change_m_owned_for_sell st tile_to_sell.cost tile_to_sell in
      {st with active_players = new_active_players;
               board = new_board}
    end
  (*When trying to sell a tile that is mortgaged.*)
  else if curr_house= 0 && condition && tile_to_sell.is_mortgaged then
    begin
      let new_board = List.map (fun x -> if x.t_id = tile_to_sell.t_id then
                                            {x with owner = 100; is_mortgaged = false}
                                         else x) st.board in
      let new_active_players =
        change_m_mort_for_sell st (tile_to_sell.cost/2) tile_to_sell in
      {st with active_players = new_active_players;
               board = new_board}
    end
  else
    raise (Invalid_Command "You cannot sell this tile")

(* HELPERS FOR MORTGAGE COMMAND*)
(*[mortgage st index] is a new state where the player has mortgaged the tile with
 * [index]*)
let mortgage st index =
  let tile_to_sell = List.nth st.board index in
  let condition = (tile_to_sell.owner = (curr_turn_id st)) in
  let condition_2 = (tile_to_sell.is_mortgaged = false) in
  if tile_to_sell.curr_house = 0 && condition && condition_2 then
    begin
      let new_board =
        List.map
          (fun x -> if x.t_id = tile_to_sell.t_id then
                       {x with is_mortgaged = true}
                     else x) st.board in
      let prop_removed =
        List.filter
          (fun x -> x <> (curr_tile st))
          (curr_turn_player st).owned_property in
      let new_active_players =
        List.map
          (fun x -> if x.id = (curr_turn_id st) then
                      {x with money = x.money + (tile_to_sell.cost)/2;
                              mortgaged_property = tile_to_sell::x.mortgaged_property;
                              owned_property = prop_removed}
                    else x) st.active_players in
      {st with active_players = new_active_players;
               board = new_board;}
    end
  else
    raise (Invalid_Command "You cannot mortgage this tile")

(* HELPERS FOR QUIT COMMAND*)
let reset_tile st =
  List.map (fun x -> if x.owner = (curr_turn_id st) then
               {x with owner = 100;
                       curr_house = 0;
                       is_mortgaged = false;} else x) st.board

(* [quit st] is the new state with current player removed from the active player
 * list. *)
let quit st =
  let new_board = reset_tile st in
  let new_active_players =
    List.filter (fun x -> (curr_turn_player st) <> x) st.active_players in
  let new_winner =
    if (List.length new_active_players) = 1 then List.hd new_active_players
    else empty_player in
  let new_game_fin =  ((List.length new_active_players) = 1) in
  {st with active_players = new_active_players; board = new_board;
           current_turn = st.current_turn mod (List.length new_active_players);
           winner = new_winner;
           game_finished = new_game_fin;}

(* [property_b_val lst acc] returns the total value of the player's owned property
 * buildings *)
let rec property_b_val lst acc =
  match lst with
  | [] -> acc
  | h::t ->
    begin
      match h.curr_house with
      | 0 -> property_b_val t acc
      | 5 -> property_b_val t (acc + (h.hotel_cost + h.house_cost * 4))
      | n -> property_b_val t (acc + h.house_cost * n)
    end

(* [property_t_val lst acc] returns the total value of the player's owned property
 * tiles *)
let rec property_t_val lst acc =
  match lst with
  | [] -> acc
  | h::t -> property_t_val t (acc + h.cost)

(*[enough_m_and_p st payment] returns true if current player has enough money.
 * Otherwise, false.*)
let enough_m_and_p st =
   ((curr_turn_player st).money +
   ((property_t_val (curr_turn_player st).owned_property 0) +
    (property_b_val (curr_turn_player st).owned_property 0)/2) >= 0)

(* HELPERS FOR ENDING TURN COMMAND*)
(*[end_turn st] is [st] with current_turn moved to the next active player in
 * [st.active_players]. Raise error if the player is trying to end with
 * negative amount of money. *)
let end_turn st =
  if ((curr_turn_player st).money < 0) && (enough_m_and_p st) then
    raise (Invalid_Command ("You have negative amount of money.\n" ^
                            "You have enough properties to recover.\n" ^
                            "Sell your property before moving on."))

  else if ((curr_turn_player st).money < 0) then
    raise (Invalid_Command ("You have negative amount of money.\n" ^
                            "You do not have enough properties to recover.\n" ^
                            "You should quit."))

  else if st.double then
    {st with double = false;turn_count = st.turn_count + 1}

  else
    let new_active_players =
      List.map
        (fun x -> if x.id = (curr_turn_id st) then
                    {x with card_drawn = false}
                  else x) st.active_players in
    {st with
     current_turn = ((find_index (curr_turn_player st) st.active_players 0)+1)
                    mod (List.length st.active_players);
     active_players = new_active_players;
     turn_count = st.turn_count + 1}

(* HELPERS FOR COLLECTING TAX*)
(*[collect_tax st m] is a new state with current player's money altered by [m]
 * and tax_collected altered by [m]*)
let collect_tax st m =
  let new_active_players = (change_player_money st (-m)) in
  {st with active_players = new_active_players;
           tax_collected = st.tax_collected + m;}

(*[pay_tax st payment]. If current player's money is less than payment, check
 * if they are bankrupt. Otherwise, subtract the player's money with necessary
 * payment.*)
let pay_tax st payment c =
  let new_st = collect_tax st payment in
  match c with
  | Turn_start -> new_st
  | Sell_House (n, idx) -> sell_houses st n idx
  | Sell_Hotel idx -> sell_hotel st idx
  | Sell_Tile idx -> sell_tile st idx
  | Mortgage idx -> mortgage st idx
  | EndTurn -> end_turn st
  | Inventory -> st
  | Quit -> quit st
  | _ -> raise (Invalid_Command "You cannot choose this command")

(* HELPER TO CHECK IF SET IS OWNED*)
(*[check_set_utilities st player] returns true if all of the tiles
 * are owned by [player]. *)
let check_set_utilities st player =
  let tile_1 = List.nth st.board 12 in
  let tile_2 = List.nth st.board 28 in

  (tile_1.owner = player) && (tile_2.owner = player)

(* [check_set_train_station st player] returns the number of owned train station
 * owned by [player]. *)
let check_set_train_station st player=
  let tile_1 = List.nth st.board 5 in
  let tile_2 = List.nth st.board 15 in
  let tile_3 = List.nth st.board 25 in
  let tile_4 = List.nth st.board 35 in

  (if tile_1.owner = player then 1 else 0) +
  (if tile_2.owner = player then 1 else 0) +
  (if tile_3.owner = player then 1 else 0) +
  (if tile_4.owner = player then 1 else 0)

(* [check_set_1 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_1 st player =
  let tile_1 = List.nth st.board 1 in
  let tile_2 = List.nth st.board 3 in
  (tile_1.owner = player) && (tile_2.owner = player)

(* [check_set_2 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_2 st player=
  let tile_1 = List.nth st.board 6 in
  let tile_2 = List.nth st.board 8 in
  let tile_3 = List.nth st.board 9 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_3 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_3 st player=
  let tile_1 = List.nth st.board 11 in
  let tile_2 = List.nth st.board 13 in
  let tile_3 = List.nth st.board 14 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_4 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_4 st player=
  let tile_1 = List.nth st.board 16 in
  let tile_2 = List.nth st.board 18 in
  let tile_3 = List.nth st.board 19 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_5 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_5 st player=
  let tile_1 = List.nth st.board 21 in
  let tile_2 = List.nth st.board 23 in
  let tile_3 = List.nth st.board 24 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_6 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_6 st player=
  let tile_1 = List.nth st.board 26 in
  let tile_2 = List.nth st.board 27 in
  let tile_3 = List.nth st.board 29 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_7 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_7 st player=
  let tile_1 = List.nth st.board 31 in
  let tile_2 = List.nth st.board 32 in
  let tile_3 = List.nth st.board 34 in
  ((tile_1.owner = player) && (tile_2.owner = player)) ||
  ((tile_2.owner = player) && (tile_3.owner = player)) ||
  ((tile_1.owner = player) && (tile_3.owner = player))

(* [check_set_8 st player] returns true if every tile of set 1 is owned by
 * [player]. Otherwise, returns false *)
let check_set_8 st player=
  let tile_1 = List.nth st.board 37 in
  let tile_2 = List.nth st.board 39 in
  (tile_1.owner = player) && (tile_2.owner = player)

let check_property_set st owner =
  match (curr_tile st).set with
  | 1 -> check_set_1 st owner
  | 2 -> check_set_2 st owner
  | 3 -> check_set_3 st owner
  | 4 -> check_set_4 st owner
  | 5 -> check_set_5 st owner
  | 6 -> check_set_6 st owner
  | 7 -> check_set_7 st owner
  | 8 -> check_set_8 st owner
  | _ -> failwith "Impossible."

(*FACTORED OUT COMMANDS*)
(*[basic_command c st new_st] is a new state with [c] done on [st] if c is not
 * Turn_start. *)
let basic_command c st new_st =
  match c with
  | Turn_start -> new_st
  | Sell_House (n, idx) -> sell_houses st n idx
  | Sell_Hotel idx -> sell_hotel st idx
  | Sell_Tile idx -> sell_tile st idx
  | Mortgage idx -> mortgage st idx
  | EndTurn -> end_turn st
  | Inventory -> st
  | Quit -> quit st
  | _ -> raise (Invalid_Command "You cannot choose this command")

(*[basic_command_with_buy_tile c st] is a new state with [c] done on [st] if c
 * is not Turn_start*)
let basic_command_with_buy_tile c st new_st =
  match c with
  | Turn_start -> new_st
  | Sell_House (n, idx) -> sell_houses st n idx
  | Sell_Hotel idx -> sell_hotel st idx
  | Sell_Tile idx -> sell_tile st idx
  | Mortgage idx -> mortgage st idx
  | Buy_tile -> buy_tile st
  | EndTurn -> end_turn st
  | Inventory -> st
  | Quit -> quit st
  | _ -> raise (Invalid_Command "You cannot choose this command")

(*[basic_command_with_buy_mort c st] is a new state with [c] done on [st]*)
let basic_command_with_buy_mort c st new_st=
  match c with
  | Turn_start -> new_st
  | Buy_mortgaged -> buy_mortgaged st
  | Sell_House (n, idx) -> sell_houses st n idx
  | Sell_Hotel idx -> sell_hotel st idx
  | Sell_Tile idx -> sell_tile st idx
  | Mortgage idx -> mortgage st idx
  | EndTurn -> end_turn st
  | Inventory -> st
  | Quit -> quit st
  | _ -> raise (Invalid_Command "You cannot choose this command")

(* HELPERS FOR PAYING RENT*)
(*[collect_tax st m] is a new state with current player's money decreased by [m]
 * and the owner of the property increased by [m]*)
let collect_rent st m owner_id=
  let new_active_players = change_player_money_for_rent st m owner_id in
  {st with active_players = new_active_players;}

(*[rent_of_houses tile set_owned] deteremines which rent that the player has to pay.*)
let rent_of_houses tile set_owned =
  match tile.curr_house with
  | 0 ->
    if set_owned then tile.set_rent else tile.initial_rent
  | 1 -> tile.rent_1_house
  | 2 -> tile.rent_2_house
  | 3 -> tile.rent_3_house
  | 4 -> tile.rent_4_house
  | 5 -> tile.rent_hotel
  | _ -> failwith "Impossible."

(*[pay_rent st payment]. If current player's money is less than payment, check
 * if they are bankrupt. Otherwise, subtract the player's money with necessary
 * payment.*)
let pay_rent st payment owner_id c =
  if (curr_tile st).is_mortgaged then
    basic_command c st st

  else
    let new_st = collect_rent st payment owner_id in
    basic_command c st new_st

(*HELPERS FOR TRAIN STATION/ UTILITIES / PROPERTIES*)
(*[train_station st c] is a new state where [c] is executed on [st].
 * Current location of the player must be a train station*)
let train_station st c =
  let owner_of_tile = (curr_tile st).owner in
  (*if the tile belongs to the bank*)
  if owner_of_tile = 100 then
    basic_command_with_buy_tile c st st

  (*if the tile belongs to the someone else*)
  else if owner_of_tile <> (curr_turn_id st) then
    (* 25 is the initial_rent of train station *)
    let number_of_ts = float_of_int (check_set_train_station st owner_of_tile) in
    let payment = (25.0 *. (2.0 ** number_of_ts)) in
    pay_rent st (int_of_float (payment)) owner_of_tile c

  (*if the tile belongs to the current_player*)
  else
    begin
      (*if the current tile is not mortgaged.*)
      if not (curr_tile st).is_mortgaged then
        basic_command c st st

      (*if the current tile is mortgaged, you can only re-buy the tile.*)
      else
        basic_command_with_buy_mort c st st
    end

(*[utilities st c] is a new state where [c] is executed on [st].
 * Current location of the player must be a utility*)
let utilities st c =
  let owner_of_tile = (curr_tile st).owner in
  (*if the tile belongs to the bank*)
  if owner_of_tile = 100 then
    basic_command_with_buy_tile c st st

  (*if the tile belongs to the someone else*)
  else if owner_of_tile <> (curr_turn_id st) then
    let dice_sum = fst_dice(st) + snd_dice(st) in
    if check_set_utilities st owner_of_tile then
      pay_rent st (10 * dice_sum) owner_of_tile c
    else
      pay_rent st (4 * dice_sum) owner_of_tile c

  (*if the tile belongs to the current_player*)
  else
    begin
      (*if the current tile is not mortgaged.*)
      if not (curr_tile st).is_mortgaged then
        basic_command c st st

      (*if the current tile is mortgaged, you can only re-buy the tile.*)
      else
        basic_command_with_buy_mort c st st
    end

(*[properties st c] is a new state where [c] is executed on [st].
 * Current location of the player must be a property*)
let properties st c =
    let owner_of_tile = (curr_tile st).owner in
    (*if the tile belongs to the bank*)
    if owner_of_tile = 100 then
      basic_command_with_buy_tile c st st

    (*if the tile belongs to the someone else*)
    else if owner_of_tile <> (curr_turn_id st) then
      let set_owned = check_property_set st owner_of_tile in
      let rent_to_pay = rent_of_houses (curr_tile st) set_owned in
      pay_rent st rent_to_pay owner_of_tile c

    (*if the tile belongs to the current_player*)
    else
      begin
        (*if the current tile is not mortgaged.*)
        if not (curr_tile st).is_mortgaged then
          match c with
          (*You can only buy houses if all set is owned.*)
          | Turn_start -> st
          | Buy_houses n ->
            if check_property_set st (curr_turn_id st) then
              buy_houses st n
            else
              raise (Invalid_Command "You need to own every set tile to buy the house")
          (*You can only buy hotel if this tile has 4 houses.*)
          | Buy_hotel ->
            if (curr_tile st).curr_house = 4 then
              buy_hotel st
            else
              raise (Invalid_Command "You need to own four houses to buy the hotel")
          | Sell_House (n, idx) -> sell_houses st n idx
          | Sell_Hotel idx -> sell_hotel st idx
          | Sell_Tile idx -> sell_tile st idx
          | Mortgage idx -> mortgage st idx
          | EndTurn -> end_turn st
          | Inventory -> st
          | Quit -> quit st
          | _ -> raise (Invalid_Command "You cannot choose this command")

        (*if the current tile is mortgaged, you can only re-buy the tile.*)
        else
          basic_command_with_buy_mort c st st
      end

(* HELPERS FOR COMMUNITY / CHANCE CARDS*)
(*[pay_money_to_otherwise st payment]. Subtract the player's money with necessary
 * payment and add other player's money with [m]*)
let pay_money_to_others st m c=
  let num_of_players = (List.length st.active_players-1) in
  let new_active_players =
    List.map
      (fun x -> if x.id <> (curr_turn_id st) then
                  {x with money = x.money + m}
                else
                  {x with money = x.money - (m * num_of_players);}) st.active_players in
  let new_st = {st with active_players = new_active_players} in

  basic_command c st new_st

(*[pay_money_to_bank st payment]. Subtract the player's money with necessary
 * payment.*)
let pay_money_to_bank st m c=
  let new_active_players =
    List.map (fun x -> if x.id = (curr_turn_id st) then
                         {x with money = x.money - m;}
                       else x) st.active_players in
  let new_st = {st with active_players = new_active_players} in

  basic_command c st new_st

(*[num_houses] is the number of houses that the player owns*)
let num_houses st =
  let rec num_houses_helper lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      if h.owner = (curr_turn_id st) then
        num_houses_helper t (acc + (if h.curr_house < 5 then h.curr_house else 0))
      else num_houses_helper t acc in

  num_houses_helper st.board 0

(*[num_hotel] is the number of hotels that the player owns*)
let num_hotel st =
  let rec num_hotel_helper lst acc =
    match lst with
    | [] -> acc
    | h::t ->
      if h.owner = (curr_turn_id st) then
        num_hotel_helper t (acc + (if h.curr_house = 5 then 1 else 0))
      else num_hotel_helper t acc in

  num_hotel_helper st.board 0

(*[take_effect] is a new state with the changes according to [effect] has been made and
 * does [c] on that new_state.*)
let take_effect st effect c =
  (*Group 1==========================*)
  if effect.wealth_gain <> 0 then
    (*Gaining [wealth_gain] per every player.*)
    if effect.multiplier = true then
      let num_of_players = List.length st.active_players in
      let new_active_players =
        List.map
          (fun x -> if x.id = (curr_turn_id st) then
                      {x with money = x.money + (num_of_players * effect.wealth_gain);}
                    else x) st.active_players in
      let new_st = {st with active_players = new_active_players} in

      basic_command c st new_st

    (*Gaining just [wealth_gain]*)
    else
      let new_active_players =
        List.map
          (fun x -> if x.id = (curr_turn_id st) then
                      {x with money = x.money + effect.wealth_gain;}
                    else x) st.active_players in
      let new_st = {st with active_players = new_active_players} in
      basic_command c st new_st

  (*Group 2==========================*)
  else if effect.wealth_loss <> 0 then
    (*Losing [wealth_loss] per every player.*)
    if effect.multiplier = true then
      pay_money_to_others st effect.wealth_loss c

    (*Losing just [wealth_loss]*)
    else
      pay_money_to_bank st effect.wealth_loss c

  (*Group 3==========================*)
  else if effect.house_repair <> 0 then
    let houses = num_houses st in
    let hotel = num_hotel st in
    let payment = (houses * effect.house_repair) + (hotel * effect.hotel_repair) in
    pay_money_to_bank st payment c

  (*Group 4==========================*)
  else if effect.get_out_of_jail then
    let new_active_players =
      List.map (fun x -> if x.id = (curr_turn_id st) then
                            {x with jail_card = true;}
                         else x) st.active_players in
    let new_st = {st with active_players = new_active_players} in

    basic_command c st new_st

  (*Group 5==========================*)
  else (*if effect.new_location <> 0 then*)
    (*Advance to Go*)
    begin
      if effect.new_location = 0 then
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                                  {x with current_location = 0;
                                          money = x.money + 200;}
                             else x) st.active_players in
        let new_st = {st with active_players = new_active_players} in
        basic_command c st new_st

      (*Advance to Jail*)
      else if effect.new_location = 10 then
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with current_location = (if x.jail_card then x.current_location
                                            else 10);
                        jail_count = (if x.jail_card then 0 else 2);
                        jail_card = false;} else x) st.active_players in

        let new_st = {st with active_players = new_active_players} in
        basic_command c st new_st

      (*Advance to Last Railroad*)
      else if effect.new_location = 35 then
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with money = (if x.current_location > 35 then x.money + 200
                                 else x.money);
                        current_location = 35;} else x) st.active_players in
        let new_st = {st with active_players = new_active_players} in
        let owner_of_tile = (curr_tile new_st).owner in
        (*if the tile belongs to the bank*)
        if owner_of_tile = 100 then
          basic_command_with_buy_tile c st new_st

        (*if the tile belongs to the someone else*)
        else if owner_of_tile <> (curr_turn_id new_st) then
          (* 25 is the initial_rent of train station *)
          let number_of_ts = float_of_int (check_set_train_station st owner_of_tile) in
          let payment = (25.0 *. (2.0 ** number_of_ts)) in
          pay_rent new_st (int_of_float (payment)) owner_of_tile c

        (*if the tile belongs to the current_player*)
        else
          (*if the current tile is not mortgaged.*)
        if not (curr_tile new_st).is_mortgaged then
          basic_command c st new_st

        (*if the current tile is mortgaged, you can only re-buy the tile.*)
        else
          basic_command_with_buy_mort c st new_st

      (* Advance to nearest Railroad*)
      else if effect.new_location = 50 then
        let nearest = ((curr_turn_player st).current_location / 10) * 10 + 5 in
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with money = (if x.current_location > nearest then x.money + 200
                                 else x.money);
                        current_location = nearest;} else x) st.active_players in

        let new_st = {st with active_players = new_active_players} in
        let owner_of_tile = (curr_tile new_st).owner in
        (*if the tile belongs to the bank*)
        if owner_of_tile = 100 then
          basic_command_with_buy_tile c st new_st

        (*if the tile belongs to the someone else*)
        else if owner_of_tile <> (curr_turn_id new_st) then
          (* 25 is the initial_rent of train station *)
          let number_of_ts = float_of_int (check_set_train_station st owner_of_tile) in
          let payment = (25.0 *. (2.0 ** (number_of_ts -. 1.0))) in
          pay_rent new_st (int_of_float (payment)) owner_of_tile c

        (*if the tile belongs to the current_player*)
        else
          begin
            (*if the current tile is not mortgaged.*)
            if not (curr_tile new_st).is_mortgaged then
              basic_command c st new_st

            (*if the current tile is mortgaged, you can only re-buy the tile.*)
            else
              basic_command_with_buy_mort c st new_st
          end

      (* Advance to nearest Utilities*)
      else if effect.new_location = 60 then
        let nearest = if (curr_turn_player st).current_location < 20 then 12 else 28 in
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with money = (if x.current_location > nearest then x.money + 200
                                 else x.money);
                        current_location = nearest;} else x) st.active_players in

        let new_st = {st with active_players = new_active_players} in
        let owner_of_tile = (curr_tile new_st).owner in
        (*if the tile belongs to the bank*)
        if owner_of_tile = 100 then
          basic_command_with_buy_tile c st new_st

        (*if the tile belongs to the someone else*)
        else if owner_of_tile <> (curr_turn_id new_st) then
          let dice_sum = fst_dice(new_st) + snd_dice(new_st) in
          if check_set_utilities new_st owner_of_tile then
            pay_rent new_st (20 * dice_sum) owner_of_tile c
          else
            pay_rent new_st (8 * dice_sum) owner_of_tile c

        (*if the tile belongs to the current_player*)
        else
          begin
            (*if the current tile is not mortgaged.*)
            if not (curr_tile new_st).is_mortgaged then
              basic_command c st new_st

            (*if the current tile is mortgaged, you can only re-buy the tile.*)
            else
              basic_command_with_buy_mort c st new_st
          end

      (* Advance to properties*)
      else
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with money = (if x.current_location > effect.new_location then
                                   x.money + 200
                                 else x.money);
                        current_location = effect.new_location;} else x)
            st.active_players in

        let new_st = {st with active_players = new_active_players} in
        let owner_of_tile = (curr_tile new_st).owner in
        (*if the tile belongs to the bank*)
        if owner_of_tile = 100 then
          basic_command_with_buy_tile c st new_st

        (*if the tile belongs to the someone else*)
        else if owner_of_tile <> (curr_turn_id new_st) then
          let set_owned = check_property_set new_st owner_of_tile in
          let rent_to_pay = rent_of_houses (curr_tile new_st) set_owned in
          pay_rent new_st rent_to_pay owner_of_tile c

        (*if the tile belongs to the current_player*)
        else
          begin
            (*if the current tile is not mortgaged.*)
            if not (curr_tile new_st).is_mortgaged then
              match c with
              (*You can only buy houses if all set is owned.*)
              | Turn_start -> new_st
              | Buy_houses n ->
                if check_property_set new_st (curr_turn_id new_st) then
                  buy_houses st n
                else
                  raise (Invalid_Command "You need to own every set tile to buy the house")
              (*You can only buy hotel if this tile has 4 houses.*)
              | Buy_hotel ->
                if (curr_tile new_st).curr_house = 4 then
                  buy_hotel st
                else
                  raise (Invalid_Command "You need to own 4 houses to buy hotel")
              | Sell_House (n, idx) -> sell_houses st n idx
              | Sell_Hotel idx -> sell_hotel st idx
              | Sell_Tile idx -> sell_tile st idx
              | Mortgage idx -> mortgage st idx
              | EndTurn -> end_turn st
              | Inventory -> st
              | Quit -> quit st
              | _ -> raise (Invalid_Command "You cannot choose this command")

            (*if the current tile is mortgaged, you can only re-buy the tile.*)
            else
              basic_command_with_buy_mort c st new_st
          end
    end

(*[roll_dice i] is a new state with the location of the player whose index is [i]
  is updated depending on the result of rolling two dice. If the player is in jail
  and has been in three consecutive turns, the player is allowed to roll the dice
  and move to the location. If the player is in jail and has been in less than three
  consecutive turns and rolls a double, the player is allowed to move. If the player
  fails to roll a double, the number of turns the player is in jail increases but
  fails to move to a new location.*)
let roll_dice st =
  if not st.game_finished then
    begin
      Random.self_init ();
      let first = (Random.int 5) + 1 in
      let second = (Random.int 5) + 1 in
      let sum = first+second in
      let double_rolled = (first=second) in

      (* if not in jail && double is not rolled *)
      if (curr_turn_player st).jail_count = 0 && (not double_rolled) then
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with money = (if x.current_location + sum >= 40 then x.money + 200
                                 else x.money);
                        current_location = (x.current_location + sum) mod 40}
                      else x) st.active_players in

        (*Changing the location of current player*)
        {st with active_players = new_active_players;
                 double = double_rolled;
                 dices = (first, second);}

      (* If double is rolled*)
      else if double_rolled then
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                       {x with money = (if x.current_location + sum >= 40 then
                                          x.money + 200
                                        else x.money);
                               current_location = (x.current_location + sum) mod 40;
                               jail_count = 0;
                       } else x) st.active_players in

          (*Changing the location of current player*)
        {st with active_players = new_active_players;
                 double = double_rolled;
                 dices = (first, second);}

      (* If in jail*)
      else
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                        {x with jail_count = x.jail_count -1}
                      else x) st.active_players in

          (*Changing the location of current player*)
        {st with active_players = new_active_players;
                 double = double_rolled;
                 dices = (first, second);}
    end

  (*game is finished.*)
  else
    raise (Invalid_Command "Game is finished. You can no longer play.")

(*END OF HELPER FUNCTIONS*)
let exec c st =
  if not st.game_finished then
    match (curr_tile st).tiletype with
    | "Go" -> basic_command c st st
    | "Property" -> properties st c
    | "Community_Chest" ->
      (*if a player has never drawn a card before*)
      if (curr_turn_player st).card_drawn = false then
        let random_num = Random.self_init (); Random.int 16 in
        let new_card = List.nth st.community_deck random_num in
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                                {x with card_drawn = true;
                                        community_drawn = new_card;}
                              else x ) st.active_players in
        let new_st = {st with active_players = new_active_players} in

        take_effect new_st new_card.comm_effect c

     (* if a player has drawn a card before and did not finish what it is asked
      * for him to do.*)
      else
        take_effect st (curr_turn_player st).community_drawn.comm_effect c

    | "Chance" ->
      (*if a player has never drawn a card before*)
      if (curr_turn_player st).card_drawn = false then
        let random_num = Random.self_init (); Random.int 16 in
        let new_card = List.nth st.chance_deck random_num in
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                                {x with card_drawn = true;
                                        chance_drawn = new_card;}
                             else x) st.active_players in
        let new_st = {st with active_players = new_active_players} in

        take_effect new_st new_card.ch_effect c

      (* if a player has drawn a card before and did not finish what it is asked
       * for him to do. Once a player finishes what he has been asked, [card_drawn]
       * will be set true*)
      else
        take_effect st (curr_turn_player st).chance_drawn.ch_effect c

    | "Tax" ->
        if (curr_tile st).is_income_tax then
          pay_tax st 200 c
        else
          pay_tax st 100 c

    | "Train_Station" -> train_station st c
    | "Jail" -> basic_command c st st
    | "Utilities" -> utilities st c
    | "Free_Parking" ->
        (*Changing money of current player.*)
        let new_st = collect_tax st (-st.tax_collected) in
        basic_command c st new_st

    | "Go_Jail" ->
      begin
        (*Changing location of curr player to Jail and change his jail count*)
        let new_active_players =
          List.map
            (fun x -> if x.id = (curr_turn_id st) then
                {x with current_location = (if x.jail_card then x.current_location
                                            else 10);
                        jail_count = (if x.jail_card then 0 else 2);
                        jail_card = false;} else x) st.active_players in
        let new_st = {st with active_players = new_active_players} in

        match c with
        | Turn_start -> new_st
        | EndTurn -> end_turn st
        | Quit -> quit st
        | _ -> raise (Invalid_Command "You cannot choose this command")
      end
    | _ -> st

  (*game is finished!! so no command should be taken.*)
  else
    raise (Invalid_Command "Game is finished. You can no longer play.")
