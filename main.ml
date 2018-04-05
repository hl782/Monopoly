open Yojson.Basic.Util
open Game
open State
open Command

(*Display Helper Functions*)
let display_buy_tile st =
  let purchased = State.curr_tile st in
  print_endline ("You purchased "^purchased.name^".\n")

let display_mortgaged st =
  let purchased = State.curr_tile st in
  print_endline ("You unmortgaged "^purchased.name^".\n")

let display_buy_houses st n =
  print_endline ("You purchased "^string_of_int(n)^" houses. \n")

let display_buy_hotel st =
  print_endline ("You purchased a hotel. \n")

let display_sell_houses st n =
  print_endline ("You sold "^string_of_int(n)^" houses. \n")

let display_sell_hotel st =
  print_endline ("You sold a hotel. \n")

let display_sell st property =
  let sold = State.get_tile_state st property in
  print_endline ("You sold "^sold.name^".\n")

let display_mort st property =
  let mortgaged = State.get_tile_state st property in
  print_endline ("You mortgaged "^mortgaged.name^".\n")

let rec list_helper (l:tile list) acc=
  match l with
  | [] -> acc
  | h::t -> list_helper t (acc^(h.name)^", ")

let display_inv st =
  let cur_player = State.get_player_state st (State.current_turn st) in
  print_endline ("You currently have "^(string_of_int (State.money cur_player))^" dollars remaining.\n");
  match List.length (State.owned_property cur_player) with
  | 0 -> print_endline ("You currently own no properties");
  | n -> print_endline ("You currently own "^(list_helper (State.owned_property cur_player) "")^".\n")

let display_upgrade st property =
  let upgraded = State.get_tile_state st property in
  print_endline ("You upgraded "^upgraded.name^".\n")

let display_winner st =
  if (List.length (State.active_players st)) = 1 then
    print_endline("The winner is "^State.name (List.hd (State.active_players st))^".\n")
  else ()
(*End of Display Helper Functions*)


(*[REPL st] returns unit - This is the engine that runs the game.*)
let rec repl st =
  let readline = Pervasives.read_line() in
  let command = Command.parse readline in
  let next_state =
    try State.exec command st with
    | State.Invalid_Command str -> st in
  let output command =
    match command with
    | Turn_start -> ()
    | Buy_tile -> display_buy_tile next_state; display_winner next_state; repl next_state
    | Buy_mortgaged -> display_mortgaged next_state; display_winner next_state; repl next_state
    | Buy_houses x-> display_buy_houses next_state x; display_winner next_state; repl next_state
    | Buy_hotel -> display_buy_hotel next_state; display_winner next_state; repl next_state
    | Sell_House (x,y) -> display_sell_houses next_state x; display_winner next_state; repl next_state
    | Sell_Hotel x-> display_sell_hotel next_state; display_winner next_state; repl next_state
    | Sell_Tile x -> display_sell next_state x; display_winner next_state; repl next_state
    | Mortgage z -> display_mort next_state z; display_winner next_state; repl next_state
    | Inventory -> display_inv next_state; display_winner next_state; repl next_state
    | EndTurn -> print_endline("Ending Turn. \n"); display_winner next_state; repl next_state
    | DoNothing -> display_winner next_state; repl next_state;
    | Quit -> print_endline("Good bye!")
  in
  output command


(* [play_game t c cc n] plays the game with tiles [t], chance cards [c]
   community chest cars [cc] and [n] number of players. *)
let play_game t c cc n =
  print_endline(t); print_endline(c); print_endline(cc);
  let emptyjson_object = `Assoc [("hl782", `List [])] in
  let empty_state = State.empty_state in

  (* Error Checking Functions that handle Json & Type Errors*)
  let check_exceptions file_name =
    try (Yojson.Basic.from_file file_name) with
    | Yojson.Json_error _ ->  print_endline "1";emptyjson_object;
    | Sys_error _ -> print_endline "2";emptyjson_object;
  in

  let check_exceptions_init t_name c_name cc_name num_players =
    try (State.init_state t_name c_name cc_name (int_of_string(num_players))) with
    | Yojson.Basic.Util.Type_error _ -> empty_state
  in
  (* End of Error Checking Functions *)

  let new_tile_json = check_exceptions t in
  let new_chance_json = check_exceptions c in
  let new_cc_json = check_exceptions cc in

  if (new_tile_json = emptyjson_object) || (new_chance_json = emptyjson_object) ||
     (new_cc_json = emptyjson_object) then print_endline "JSON Error - Will quit game."
  else
    let new_state = check_exceptions_init new_tile_json new_chance_json new_cc_json n in
    if (new_state = empty_state) then print_endline "Exception Error - Will quit game."
    else print_endline "Welcome to the Game of monopoly!";
    repl new_state
(*
(* [main ()] starts the REPL, which prompts for a game to play.
 * You are welcome to improve the user interface, but it must
 * still prompt for a game to play rather than hardcode a game file. *)
let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the Monopoly Game engine.\n");
  print_endline "Please enter the name of the file that contains tile, chance cards,
  community chest cards you want to load, along with the number of players to play the game.\n";
  print_string  "> ";
  match read_line (), read_line (), read_line (), read_line () with
  | exception End_of_file -> ()
  | numplayers, community, chance, tile -> play_game tile chance community numplayers

let () = main ()*)
