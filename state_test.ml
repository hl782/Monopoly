open OUnit2
open State
open Command
open Game

let tiles = Yojson.Basic.from_file "board.json"
let chance = Yojson.Basic.from_file "chance.json"
let community = Yojson.Basic.from_file "comm.json"
let initial_state = State.init_state tiles chance community 2



let p1 = State.get_player_state initial_state 0

let p2 = State.get_player_state initial_state 1

let test_roll_dice n m st =
  if not st.game_finished then
    begin
      let first = n in
      let second = m in
      let sum = first+second in
      let double_rolled = (first=second) in

      (* if not in jail && double is not rolled *)
      if (State.curr_turn_player st).jail_count = 0 && (not double_rolled) then
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                       {x with money = (if x.current_location + sum >= 40 then x.money + 200 else x.money);
                               current_location = (x.current_location + sum) mod 40
                       } else x) st.active_players in
        let new_state =
          (*Changing the location of current player*)
          {st with active_players = new_active_players; double = double_rolled; dices = (first, second);} in
        new_state

      (* If double is rolled*)
      else if double_rolled then
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then
                       {x with money = (if x.current_location + sum >= 40 then x.money + 200 else x.money);
                               current_location = (x.current_location + sum) mod 40; jail_count = 0;
                       } else x) st.active_players in
        let new_state =
          (*Changing the location of current player*)
          {st with active_players = new_active_players; double = double_rolled; dices = (first, second);} in
        new_state

      (* If in jail*)
      else
        let new_active_players =
          List.map (fun x -> if x.id = (curr_turn_id st) then {x with jail_count = x.jail_count -1} else x
                   ) st.active_players in
        let new_state =
          (*Changing the location of current player*)
          {st with active_players = new_active_players; double = double_rolled; dices = (first, second);} in
        new_state
    end

  (*game is finished.*)
  else
    raise (Invalid_Command "Game is finished. You can no longer play.")


(*[Testing Buy_Tile]*)
let buy_tile_tests =
  let buy1_state = test_roll_dice 2 4 initial_state |> State.exec Command.Buy_tile in
  let changed_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1} else x) initial_state.board in
  let new_active_players = List.map (fun x -> {x with money = 50;}) initial_state.active_players in
  let new_init_state = {initial_state with active_players = new_active_players;} in
  let new_init_state2 = {initial_state with active_players = new_active_players; board = changed_board;} in
  [
    "Buy Tile Good Money Change" >:: (fun _ -> assert_equal 900 (buy1_state |> State.curr_turn_player |> State.money));
    "Buy Tile Good Owned Property" >:: (fun _ -> assert_equal [(initial_state |> test_roll_dice 2 4 |> curr_tile)] (buy1_state |> State.curr_turn_player |> State.owned_property));
    "Buy Tile Good tile state Change" >:: (fun _ -> assert_equal (State.get_tile_state new_init_state2 6) (buy1_state |> State.curr_tile));
    "Buy Tile Others-Owned" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot choose this command")
                            (fun _ -> State.exec Command.EndTurn buy1_state |> test_roll_dice 2 4 |> State.exec Command.Turn_start |> State.exec Command.Buy_tile ));
    "Buy Tile Self-Owned" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot choose this command")
                            (fun _ -> State.exec Command.EndTurn buy1_state |> test_roll_dice 2 4 |> State.exec Command.Turn_start |> State.exec Command.Buy_tile |> State.exec Command.EndTurn
                                      |> State.exec Command.Buy_tile ));
    "Buy Tile No Money" >:: (fun _ -> assert_raises (State.Invalid_Command "You do not have enough money to buy a tile") (fun _ -> test_roll_dice 2 4 new_init_state |> State.exec Command.Buy_tile));
  ]

(*[Buy_house and Buy_hotel] Testing*)
let buy_house_hotel_tests =
  let buy1_state = test_roll_dice 2 4 initial_state |> State.exec Command.Buy_tile in
  let buy2_state = State.exec Command.EndTurn buy1_state |> State.exec Command.EndTurn in
  let buy1_state_missing_property = test_roll_dice 1 1 buy2_state |> State.exec Command.Buy_tile in
  let buy1_state_buy_set = test_roll_dice 1 1 buy2_state |> State.exec Command.Buy_tile |>
                            test_roll_dice 1 0 |> State.exec Command.Buy_tile in

  [
    "buy_house_missing_properties" >:: (fun _ -> assert_raises (State.Invalid_Command "You need to own four houses to buy the hotel")
                                           (fun _ -> State.exec (Command.Buy_hotel) buy1_state_missing_property));
    "buy_house_good_money" >:: (fun _ -> assert_equal 630 (buy1_state_buy_set |> State.exec (Command.Buy_houses 1) |> State.curr_turn_player |> State.money));
    "buy_multiple_house_good_money" >:: (fun _ -> assert_equal 480 (buy1_state_buy_set |> State.exec (Command.Buy_houses 4) |> State.curr_turn_player |> State.money));
    "trying_to_buy_>4_houses" >:: (fun _ -> assert_raises (State.Invalid_Command "You do not have enough money to buy houses")
                                     (fun _ -> State.exec (Command.Buy_houses 6) buy1_state_buy_set));
    "trying_to_buy_5th house" >:: (fun _ -> assert_raises (State.Invalid_Command "You do not have enough money to buy houses")
                                     (fun _ -> State.exec (Command.Buy_houses 1) buy1_state_buy_set |> State.exec (Command.Buy_houses 3) |> State.exec (Command.Buy_houses 1)));
    "buy_hotel_good_money" >:: (fun _ -> assert_equal 430 (buy1_state_buy_set |> State.exec (Command.Buy_houses 4)
                                                         |> State.exec (Command.Buy_hotel) |> State.curr_turn_player |> State.money));
    "buy_hotel_without_4_houses" >:: (fun _ -> assert_raises (State.Invalid_Command "You need to own four houses to buy the hotel")
                                         (fun _ -> State.exec (Command.Buy_houses 3) buy1_state_buy_set
                                                   |> State.exec (Command.Buy_hotel)));
    "trying_to_buy_multiple_hotels" >:: (fun _ -> assert_raises (State.Invalid_Command "You need to own four houses to buy the hotel")
                                         (fun _ -> State.exec (Command.Buy_houses 4) buy1_state_buy_set
                                                   |> State.exec (Command.Buy_hotel) |> State.exec (Command.Buy_hotel)));
  ]

(*[Sell_House] Testing*)
let sell_house_tests =
  let sell_house_when_hotel_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 5} else x) initial_state.board in
  let sell_house_when_hotel_active_players = List.map (fun x -> if x.id = 1 then {x with owned_property = [(initial_state |> test_roll_dice 2 4 |> curr_tile)]} else x) initial_state.active_players in
  let sell_house_when_hotel_st = {initial_state with active_players = sell_house_when_hotel_active_players; board = sell_house_when_hotel_board} in

  let sell_house_good_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 3} else x) initial_state.board in
  let sell_house_good_changed_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 0} else x) initial_state.board in
  let sell_house_good_active_players = List.map (fun x -> if x.id = 1 then {x with owned_property = [(initial_state |> test_roll_dice 2 4 |> curr_tile)]} else x ) initial_state.active_players in
  let sell_house_good_st = {initial_state with active_players = sell_house_good_active_players; board = sell_house_good_board} in
  let sell_house_good_st_with_change = {initial_state with active_players = sell_house_good_active_players; board = sell_house_good_changed_board} in

  [
    "sell_house_good_money" >:: (fun _ -> assert_equal 1025 (test_roll_dice 2 4 sell_house_good_st
                                                             |> State.exec (Command.Sell_House (1,6))
                                                             |> State.curr_turn_player
                                                             |>State.money));

    "sell_house_good_owned_properties" >:: (fun _ -> assert_equal [(initial_state |> test_roll_dice 2 4 |> curr_tile)]
                                               (test_roll_dice 2 4 sell_house_good_st
                                                |> State.exec (Command.Sell_House (1,6))
                                                |> State.curr_turn_player
                                                |>State.owned_property));

    "sell_house_good_tile" >:: (fun _ -> assert_equal (State.get_tile_state sell_house_good_st_with_change 6)
                                   (test_roll_dice 2 4 sell_house_good_st
                                    |> State.exec (Command.Sell_House (3,6))
                                    |> State.curr_tile));

    "sell_house_when_none" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell these houses")
                                   (fun _ -> test_roll_dice 2 4 initial_state
                                             |> State.exec (Command.Sell_House (1,6))));

    "sell_house_when_hotel" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell these houses")
                                    (fun _ -> test_roll_dice 2 4 sell_house_when_hotel_st
                                              |> State.exec (Command.Sell_House (1,6))));

    "sell_house_when_n>curr_house" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell these houses")
                                  (fun _ -> test_roll_dice 2 4 sell_house_good_st
                                            |> State.exec (Command.Sell_House (5,6))))
  ]

(*[Sell_Hotel] Testing*)
let sell_hotel_tests =
  let sell_hotel_bad_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 3} else x) initial_state.board in
  let sell_hotel_bad_active_players = List.map (fun x -> if x.id = 1 then {x with owned_property = [(initial_state |> test_roll_dice 2 4 |> curr_tile)]} else x ) initial_state.active_players in
  let sell_hotel_bad_st = {initial_state with active_players = sell_hotel_bad_active_players; board = sell_hotel_bad_board} in

  let sell_hotel_good_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 5} else x) initial_state.board in
  let sell_hotel_good_active_players = List.map (fun x -> if x.id = 1 then {x with owned_property = [(initial_state |> test_roll_dice 2 4 |> curr_tile)]} else x) initial_state.active_players in
  let sell_hotel_good_st = {initial_state with active_players = sell_hotel_good_active_players; board = sell_hotel_good_board} in
  let sell_hotel_good_changed_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 1; curr_house = 0} else x) initial_state.board in
  let sell_hotel_good_st_with_change = {initial_state with active_players = sell_hotel_good_active_players; board = sell_hotel_good_changed_board} in

  [
  "sell_hotel_when_not_enough_houses" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this hotel")
                                 (fun _ -> test_roll_dice 2 4 sell_hotel_bad_st
                                           |> State.exec (Command.Sell_Hotel 6)));

  "sell_hotel_when_none" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this hotel")
                                 (fun _ -> test_roll_dice 2 4 initial_state
                                           |> State.exec (Command.Sell_Hotel 6)));

  "sell_hotel_good_money" >:: (fun _ -> assert_equal 1125 (test_roll_dice 2 4 sell_hotel_good_st
                                                           |> State.exec (Command.Sell_Hotel 6)
                                                           |> State.curr_turn_player
                                                           |>State.money));

  "sell_hotel_good_owned_properties" >:: (fun _ -> assert_equal [(initial_state |> test_roll_dice 2 4 |> curr_tile)]
                                             (test_roll_dice 2 4 sell_hotel_good_st
                                              |> State.exec (Command.Sell_Hotel 6)
                                              |> State.curr_turn_player
                                              |>State.owned_property));

  "sell_hotel_good_tile" >:: (fun _ -> assert_equal (State.get_tile_state sell_hotel_good_st_with_change 6)
                                 (test_roll_dice 2 4 sell_hotel_good_st
                                  |> State.exec (Command.Sell_Hotel 6)
                                  |> State.curr_tile));

]

let sell_tile_tests =
  let changed_board_no_building = List.map (fun x -> if x.t_id = 3 then {x with owner = 1} else x) initial_state.board in
  let tile_owned_player = List.map (fun x -> if x.id = 1 then {x with owned_property = [(initial_state |> test_roll_dice 1 2 |> curr_tile)]} else x ) initial_state.active_players in
  let no_building_state = {initial_state with active_players = tile_owned_player; board = changed_board_no_building;} in
  let changed_board_houses = List.map (fun x -> if x.t_id = 3 then {x with owner = 1; curr_house = 1} else x) initial_state.board in
  let houses_state = {initial_state with active_players = tile_owned_player;  board = changed_board_houses;} in
  let changed_board_hotel = List.map (fun x -> if x.t_id = 3 then {x with owner = 1; curr_house = 5} else x) initial_state.board in
  let hotel_state = {initial_state with active_players = tile_owned_player; board = changed_board_hotel;} in
  [
    "Sell Tile Good Money Change" >:: (fun _ -> assert_equal 1060 (test_roll_dice 1 2 no_building_state |> State.exec (Command.Sell_Tile 3) |> State.curr_turn_player |> State.money));
    "Sell Tile Good Owned Property" >:: (fun _ -> assert_equal [] (test_roll_dice 1 2 no_building_state |> State.exec (Command.Sell_Tile 3) |> State.curr_turn_player |> State.owned_property));
    "Sell Tile Good tile state Change" >:: (fun _ -> assert_equal (State.get_tile_state initial_state 3) (test_roll_dice 1 2 no_building_state |> State.exec (Command.Sell_Tile 3) |> State.curr_tile));
    "Sell Tile Others-Owned" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                     (fun _ -> State.exec Command.EndTurn no_building_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 3)));
    "Sell Tile No Owner" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                 (fun _ -> initial_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 8)));
    "Sell Tile Chance Card" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                    (fun _ -> initial_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 7)));
    "Sell Tile Community Chest" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                   (fun _ -> initial_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 17)));
    "Sell Tile Others-Owned With Houses" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                                 (fun _ -> State.exec Command.EndTurn houses_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 3)));
    "Sell Tile Others-Owned With Hotel" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                     (fun _ -> State.exec Command.EndTurn hotel_state |> test_roll_dice 1 1 |> State.exec Command.Turn_start |> State.exec (Command.Sell_Tile 3)));
    "Sell Tile With Houses" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                    (fun _ -> houses_state |> test_roll_dice 1 1 |> State.exec (Command.Sell_Tile 3)));
    "Sell Tile With Hotel" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot sell this tile")
                                    (fun _ -> hotel_state |> test_roll_dice 1 1 |> State.exec (Command.Sell_Tile 3)));

  ]

(*[Turn_start] Testing*)
let turn_start_tests =
  (*free_parking*)
  let free_parking_active_player = List.map (fun x -> if x.id = 1 then {x with current_location = 11;} else x ) initial_state.active_players in
  let free_parking_st = {initial_state with active_players = free_parking_active_player;
                                            tax_collected = 200;} in

  (*tile_rent*)
  let tile_rent_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 2;} else x) initial_state.board in
  let tile_rent_st = {initial_state with board = tile_rent_board} in

  (*set_rent*)
  let set_rent_board = List.map (fun x -> if (x.t_id = 6) || (x.t_id = 8) || (x.t_id = 9) then {x with owner = 2;} else x) initial_state.board in
  let set_rent_st = {initial_state with board = set_rent_board} in

  (*hote_rent*)
  let hotel_rent_board = List.map (fun x -> if x.t_id = 6 then {x with owner = 2;curr_house = 5} else x) initial_state.board in
  let hotel_rent_st = {initial_state with board = hotel_rent_board;} in

  (*wealth_gain_chance*)
  let wealth_gain_chance_active_player = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                                     chance_drawn = (List.nth (initial_state.chance_deck) 5);
                                                                                     current_location = 7} else x) initial_state.active_players in
  let wealth_gain_chance_st = {initial_state with active_players = wealth_gain_chance_active_player} in

  (*nearest_ts_chance*)
  let nearest_ts_board = List.map (fun x -> if (x.t_id = 5) || (x.t_id=15) || (x.t_id=25) then {x with owner = 2;} else x) initial_state.board in
  let nearest_ts_active_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                               chance_drawn = (List.nth (initial_state.chance_deck) 4);
                                                                               current_location = 7} else x) initial_state.active_players in
  let nearest_ts_st = {initial_state with active_players = nearest_ts_active_players;
                                          board = nearest_ts_board} in

  (*nearest_ts_chance*)
  let nearest_ut_board = List.map (fun x -> if (x.t_id = 12) || (x.t_id=28) then {x with owner = 2;} else x) initial_state.board in
  let nearest_ut_active_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                               chance_drawn = (List.nth (initial_state.chance_deck) 3);
                                                                               current_location = 7} else x) initial_state.active_players in
  let nearest_ut_st = {initial_state with active_players = nearest_ut_active_players;
                                          board = nearest_ut_board;
                                          dices = (3,4);} in

  (*go_to_jail_chance*)
  let go_jail_active_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                           chance_drawn = (List.nth (initial_state.chance_deck) 8);
                                                                           current_location = 7} else x) initial_state.active_players in
  let go_jail_st = {initial_state with active_players = go_jail_active_players} in

  (*go_to_jail_chance_when_jail_card*)
  let go_jail_jail_card_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                              chance_drawn = (List.nth (initial_state.chance_deck) 8);
                                                                              current_location = 7;
                                                                              jail_card = true} else x) initial_state.active_players in
  let go_jail_jail_card_st = {initial_state with active_players = go_jail_jail_card_players} in

  (*repair_chance*)
  let repair_chance_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                          chance_drawn = (List.nth (initial_state.chance_deck) 9);
                                                                          current_location = 7;} else x) initial_state.active_players in
  let repair_chance_board = List.map (fun x -> if (x.t_id = 6) || (x.t_id = 8) then {x with owner = 1; curr_house = 5}
                                               else if (x.t_id = 9) then {x with owner = 1; curr_house = 3}
                                               else x) initial_state.board in
  let repair_chance_st = {initial_state with active_players = repair_chance_players;
                                             board = repair_chance_board} in

  (*pay each player 50*)
  let pay_each_50_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                        chance_drawn = (List.nth (initial_state.chance_deck) 13);
                                                                        current_location = 7;} else x) initial_state.active_players in
  let pay_each_50_st = {initial_state with active_players = pay_each_50_players} in

  (*trying to end with neg money*)
  let ending_with_neg_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                            chance_drawn = (List.nth (initial_state.chance_deck) 13);
                                                                            current_location = 7;
                                                                            money = 0;} else x) initial_state.active_players in
  let ending_with_neg_st= {initial_state with active_players = ending_with_neg_players} in

  (*repair_comm*)
  let repair_comm_players = List.map (fun x -> if x.id = 1 then {x with card_drawn = true;
                                                                        community_drawn = (List.nth (initial_state.community_deck) 14);
                                                                        current_location = 2;} else x) initial_state.active_players in
  let repair_comm_board = List.map (fun x -> if (x.t_id = 6) || (x.t_id = 8) then {x with owner = 1; curr_house = 5}
                                               else if (x.t_id = 9) then {x with owner = 1; curr_house = 3}
                                               else x) initial_state.board in
  let repair_comm_st = {initial_state with active_players = repair_comm_players;
                                             board = repair_comm_board} in

  [
    "Changing_money_due_to_free_parking_taxcollected" >:: (fun _ -> assert_equal 0
                                                 (test_roll_dice 4 5 free_parking_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.get_tax_collected));

    "Changing_money_due_to_free_parking_playermoney" >:: (fun _ -> assert_equal 1200
                                                 (test_roll_dice 4 5 free_parking_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.curr_turn_player
                                                  |> State.money));

    "Changing_money_due_to_tax_taxcollected" >:: (fun _ -> assert_equal 200
                                        (test_roll_dice 1 3 initial_state
                                         |> State.exec (Command.Turn_start)
                                         |> State.get_tax_collected));

    "Changing_money_due_to_tax_playermoney" >:: (fun _ -> assert_equal 800
                                        (test_roll_dice 1 3 initial_state
                                         |> State.exec (Command.Turn_start)
                                         |> State.curr_turn_player
                                         |> State.money));


    "Changing_money_due_to_tile_rent_p1" >::(fun _ -> assert_equal 994
                                                 (test_roll_dice 2 4 tile_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

    "Changing_money_due_to_tile_rent_p2" >::(fun _ -> assert_equal 1006
                                                 (test_roll_dice 2 4 tile_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.exec Command.EndTurn
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

      "Changing_money_due_to_set_rent_p1" >::(fun _ -> assert_equal 988
                                                 (test_roll_dice 2 4 set_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

      "Changing_money_due_to_set_rent_p2" >::(fun _ -> assert_equal 1012
                                                 (test_roll_dice 2 4 set_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.exec Command.EndTurn
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

    "Changing_money_due_to_hotel_rent_p1" >::(fun _ -> assert_equal 450
                                                 (test_roll_dice 2 4 hotel_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

    "Changing_money_due_to_hotel_rent_p2" >::(fun _ -> assert_equal 1550
                                                 (test_roll_dice 2 4 hotel_rent_st
                                                  |> State.exec Command.Turn_start
                                                  |> State.exec Command.EndTurn
                                                  |> State.curr_turn_player
                                                  |> State.money)) ;

    "Changing_money_due_to_chance_card1" >:: (fun _ -> assert_equal 1050
                                                 (State.exec Command.Turn_start wealth_gain_chance_st
                                                  |> State.curr_turn_player
                                                  |> State.money));

    "Changing_money_due_to_chance2_p1" >:: (fun _ -> assert_equal 1100
                                            (State.exec Command.Turn_start nearest_ts_st
                                             |> State.curr_turn_player
                                             |> State.money));

   "Changing_money_due_to_chance2_p1loc" >:: (fun _ -> assert_equal 5
                                           (State.exec Command.Turn_start nearest_ts_st
                                            |> State.curr_turn_player
                                            |> State.location));

   "Changing_money_due_to_chance2_p2" >:: (fun _ -> assert_equal 1100
                                           (State.exec Command.Turn_start nearest_ts_st
                                            |> State.exec Command.EndTurn
                                            |> State.curr_turn_player
                                            |> State.money));

    "Go Jail Chance" >:: (fun _ -> assert_equal 10 ((State.exec Command.Turn_start go_jail_st
                                                     |> State.curr_turn_player)).current_location);

    "Go Jail Chance then Roll" >:: (fun _ -> assert_equal 10 ((State.exec Command.Turn_start go_jail_st
                                                               |> State.exec Command.EndTurn
                                                               |> State.exec Command.EndTurn
                                                               |> test_roll_dice 4 3
                                                               |> State.curr_turn_player)).current_location);

    "Go Jail Chance then Double Roll" >:: (fun _ -> assert_equal 18 ((State.exec Command.Turn_start go_jail_st
                                                                |> State.exec Command.EndTurn
                                                                |> State.exec Command.EndTurn
                                                                |> test_roll_dice 4 4
                                                                |> State.curr_turn_player)).current_location);

    "Go Jail Chance" >:: (fun _ -> assert_equal 7 ((State.exec Command.Turn_start go_jail_jail_card_st
                                                    |> State.curr_turn_player)).current_location);

    "Repair Chance with 2 hotels and 1 house" >:: (fun _ -> assert_equal 725
                                                      (State.exec Command.Turn_start repair_chance_st
                                                       |> State.curr_turn_player
                                                       |> State.money));

    "Pay each player 50 Player 1" >:: (fun _ -> assert_equal 950 (State.exec Command.Turn_start pay_each_50_st
                                                         |> State.curr_turn_player
                                                         |> State.money));

    "Pay each player 50 Other Players" >:: (fun _ -> assert_equal 1050 (State.exec Command.Turn_start pay_each_50_st
                                                                        |> State.exec Command.EndTurn
                                                                        |> State.curr_turn_player
                                                                        |> State.money));

    "Ending with Neg Money" >:: (fun _ -> assert_raises (State.Invalid_Command "You have negative amount of money.\nYou do not have enough properties to recover.\nYou should quit.")
                                    (fun _ -> (State.exec Command.Turn_start ending_with_neg_st
                                               |> State.exec Command.EndTurn)));

   "Repair Chance with 2 hotels and 1 house" >:: (fun _ -> assert_equal 650
                                                     (State.exec Command.Turn_start repair_comm_st
                                                      |> State.curr_turn_player
                                                      |> State.money));

    "Nearest Utilities Player 1 Money" >:: (fun _ -> assert_equal 860
                                            (State.exec Command.Turn_start nearest_ut_st
                                             |> State.curr_turn_player
                                             |> State.money));

   "Nearest Utilities Player 1 Loc" >:: (fun _ -> assert_equal 12
                                           (State.exec Command.Turn_start nearest_ut_st
                                            |> State.curr_turn_player
                                            |> State.location));

   "Nearest Utilities Player 2 Money" >:: (fun _ -> assert_equal 1140
                                           (State.exec Command.Turn_start nearest_ut_st
                                            |> State.exec Command.EndTurn
                                            |> State.curr_turn_player
                                            |> State.money));

  ]

(*[Testing Mortgage and Buy_mortgaged]*)
let mortgage_tests =
  let mort1_state = test_roll_dice 2 4 initial_state |> State.exec Command.Buy_tile in
  let mort2_state = State.exec Command.EndTurn mort1_state |> State.exec Command.EndTurn in
  let mort1_state_buy_set = test_roll_dice 1 1 mort2_state |> State.exec Command.Buy_tile |>
                            test_roll_dice 1 0 |> State.exec Command.Buy_tile in

  [
    "Mortgage Good" >:: (fun _ -> assert_equal 950
                            (mort1_state |> State.exec (Command.Mortgage 6) |> State.curr_turn_player |> State.money));
    "Unmortgage an unmortgaged Property" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot choose this command")
                                                 (fun _ -> mort1_state |> State.exec (Command.Buy_mortgaged)));
    "Unmortgage Good" >:: (fun _ -> assert_equal 900
                              (mort1_state |> State.exec (Command.Mortgage 6) |> State.exec (Command.Buy_mortgaged) |> State.curr_turn_player |> State.money));
    "Mortgage Bad" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot mortgage this tile")
                           (fun _ -> mort1_state |> State.exec (Command.Mortgage 8)));
    "Unmortgage Bad" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot choose this command")
                             (fun _ -> mort1_state |> test_roll_dice 1 1 |> State.exec (Command.Buy_mortgaged)));
    "Mortgage a Mortgaged Property" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot mortgage this tile")
                                            (fun _ -> mort1_state |> State.exec (Command.Mortgage 6) |> State.exec (Command.Mortgage 6)));
    "Mortgage A Property with a House" >:: (fun _ -> assert_raises (State.Invalid_Command "You cannot mortgage this tile")
                                               (fun _ -> mort1_state_buy_set |> State.exec (Command.Buy_houses 1) |> State.exec (Command.Mortgage 9)));
  ]

(* let new_board = List.map (fun x -> if x.t_id = (1,3,4) then owner = curr_turn)
let buy_house_state = initial_state with {board = new_board} *)
let suite =
  "Monopoly test suite"
  >::: buy_tile_tests@buy_house_hotel_tests@mortgage_tests@sell_house_tests@sell_hotel_tests@sell_tile_tests@turn_start_tests

let _ = run_test_tt_main suite
