(* Source: The base code for this program is from
 * https://ocaml.org/learn/tutorials/introduction_to_gtk.html *)

open State
open Game
open Command
open Yojson.Basic.Util
open GMain
open GdkKeysyms

(* When clicking on a tile, we need to know whether we are currently
 * trying to sell the tile, or just displaying the current tile info.
 * Thus we will need a variant to describe the current selling state.
 * Also, if we want to allow house-building from anywhere on the
 * board, this variant can be extended for such a state.
 *)
type sell_state =
  | No_Sell
  | Mortgage
  | Sell
  | Sell_House
  | Sell_Hotel

let locale = GtkMain.Main.init ()

let main () =
  (* Set up window *)
  let window = GWindow.window ~width:1200 ~height:600
      ~title:"Monopoly!" () in
  window#connect#destroy ~callback:Main.quit;

  (* Base table for the GUI *)
  let table = GPack.table ~columns:11 ~rows:11
                          ~homogeneous:true ~packing:window#add () in

  (* [add_button label x y] adds a button with text label
   * to the table in row r and column c *)
  let add_button label r c =
    GButton.button ~label:label
      ~packing:(table#attach ~top:r ~left:c ~expand:`BOTH) () in

  (* [add_button_b x y] adds a blacnk button to the table in row r and 
   * column c *)
  let add_button_b r c =
    GButton.button ~packing:(table#attach ~top:r ~left:c ~expand:`BOTH) () in

  (* TODO: accept user file instead of hardcoding file
   * TODO: need a way to input names. state may need a function to set names.
   * We need to store state as a ref so that we modify it during runtime *)
  (* Note: for GUI mode, 4 players max! *)
  let current_state =
    ref (State.init_state (Yojson.Basic.from_file "board.json")
                          (Yojson.Basic.from_file "chance.json")
                          (Yojson.Basic.from_file "comm.json") 4) in

  (* The current selling state (see definition of variant above *)
  let current_sell_state = ref (No_Sell) in

  (* [tile_name st n] is the name of tile n in game state st *)
  let tile_name st n = (State.get_tile_state st n).name in

  (* Fill the board with tiles *)
  let tile0 = add_button_b (*tile_name !current_state 0*) 10 10 in
  let tiles = Array.make 41 tile0 in
  for i = 1 to 10
  do
    tiles.(i) <- add_button_b (*tile_name !current_state i*) 10 (10-i)
  done;
  for i = 1 to 10
  do
    tiles.(i+10) <- add_button_b (*tile_name !current_state (i+10)*) (10-i) 0
  done;
  for i = 1 to 10
  do
    tiles.(i+20) <- add_button_b (*tile_name !current_state (i+20)*) 0 i
  done;
  for i = 1 to 9
  do
    tiles.(i+30) <- add_button_b (*tile_name !current_state (i+30)*) i 10
  done;
  tiles.(40) <- add_button_b (*"Jail"*) 9 1;

  (* We need to make widgets for each tile so that we can hide the tiles
   * when needed. *)
  let widget0 = new GObj.misc_ops tile0#as_widget in
  let tile_widgets = Array.make 41 widget0 in
  for i = 1 to 40
  do
    tile_widgets.(i) <- new GObj.misc_ops tiles.(i)#as_widget
  done;

  (* [hide_all ()] hides every tile on the board. *)
  let hide_all () =
    for i = 0 to 40
    do
      tile_widgets.(i)#hide ()
    done in

  (* [display_all ()] displays every tile on the board. *)
  let display_all () =
    for i = 0 to 40
    do
      tile_widgets.(i)#show ()
    done in

  (* LOGO *)
  let mo = GdkPixbuf.from_file_at_size "./images/MO.jpg" ~width:110 ~height:100 in
  let mo' = GMisc.image ~pixbuf:mo
                        ~packing:(table#attach ~top:4 ~left:3 ~expand:`BOTH) () in
  let no = GdkPixbuf.from_file_at_size "./images/NO.jpg" ~width:110 ~height:100 in
  let no' = GMisc.image ~pixbuf:no
                        ~packing:(table#attach ~top:4 ~left:4 ~expand:`BOTH) () in
  let po = GdkPixbuf.from_file_at_size "./images/PO.jpg" ~width:110 ~height:100 in
  let po' = GMisc.image ~pixbuf:po
                        ~packing:(table#attach ~top:4 ~left:6 ~expand:`BOTH) () in
  let ly = GdkPixbuf.from_file_at_size "./images/LY.jpg" ~width:110 ~height:100 in
  let ly' = GMisc.image ~pixbuf:ly
                        ~packing:(table#attach ~top:4 ~left:7 ~expand:`BOTH) () in
  let bear = GdkPixbuf.from_file_at_size "./images/BEAR.jpg" ~width:110 ~height:55 in
  let bear' = GMisc.image ~pixbuf:bear
                        ~packing:(table#attach ~top:4 ~left:5 ~expand:`BOTH) () in

  (* Setting images for the buttons
   * Note: in order to set an image, you must not fill the "label" field *)
  (* [set_tile_image fn n] sets tile n to have the image specified by 
   * filepath fn *)
  let set_tile_image fname tile_number =
    let pixbuf = GdkPixbuf.from_file_at_size fname ~width:110 ~height:45 in
    let image = GMisc.image ~pixbuf:pixbuf () in
    tiles.(tile_number)#set_image (new GObj.widget image#as_widget) in

  set_tile_image "./images/Go.jpg" 0;
  set_tile_image "./images/Mann.jpg" 1;
  set_tile_image "./images/ComChestA.jpg" 2;
  set_tile_image "./images/Warren.jpg" 3;
  set_tile_image "./images/IncomeTax.jpg" 4;
  set_tile_image "./images/ArtsQuad.jpg" 5;
  set_tile_image "./images/Ives.jpg" 6;
  set_tile_image "./images/ChanceA.jpg" 7;
  set_tile_image "./images/KingShaw.jpg" 8;
  set_tile_image "./images/Stocking.jpg" 9;
  set_tile_image "./images/JustVisiting.jpg" 10;
  set_tile_image "./images/Marthas.jpg" 11;
  set_tile_image "./images/CMS.jpg" 12;
  set_tile_image "./images/BusStopBagel.jpg" 13;
  set_tile_image "./images/Bailey.jpg" 14;
  set_tile_image "./images/EngQuad.jpg" 15;
  set_tile_image "./images/Milstein.jpg" 16;
  set_tile_image "./images/ComChestB.jpg" 17;
  set_tile_image "./images/Rand.jpg" 18;
  set_tile_image "./images/Tjaden.jpg" 19;
  set_tile_image "./images/FreeTuition2.jpg" 20;
  set_tile_image "./images/Pokeland.jpg" 21;
  set_tile_image "./images/ChanceB.jpg" 22;
  set_tile_image "./images/SevenEleven.jpg" 23;
  set_tile_image "./images/CollegetownBagel.jpg" 24;
  set_tile_image "./images/AgQuad.jpg" 25;
  set_tile_image "./images/Statler.jpg" 26;
  set_tile_image "./images/Terrace.jpg" 27;
  set_tile_image "./images/StackOverflow.jpg" 28;
  set_tile_image "./images/Macs.jpg" 29;
  set_tile_image "./images/GoToJail.jpg" 30;
  set_tile_image "./images/Klarman.jpg" 31;
  set_tile_image "./images/Uris.jpg" 32;
  set_tile_image "./images/ComChestC.jpg" 33;
  set_tile_image "./images/ClockTower.jpg" 34;
  set_tile_image "./images/LibeSlope.jpg" 35;
  set_tile_image "./images/ChanceC.jpg" 36;
  set_tile_image "./images/Carpenter.jpg" 37;
  set_tile_image "./images/SuperTax.jpg" 38;
  set_tile_image "./images/Gates.jpg" 39;
  set_tile_image "./images/AcademicHearing.jpg" 40;

  (* for reference: how to delete a button *)
  (*tiles.(0)#destroy ();
  tiles.(0) <- add_button "GO" 10 10;*)

  (* [player_info st n] is the displayed information of player n in state st.
   * TODO: what information to display? *)
  let player_info st n =
    let player_state = get_player_state st n in
    "Player : "^(State.name player_state)
    ^"\nMoney: $"^(string_of_int (State.money player_state))
    ^"\nLocation: "^(tile_name st (State.location player_state))
    ^(if jail_count player_state = 0 then ""
      else (" - Turns left in academic hearing: "^
            (string_of_int (jail_count player_state)))) in

  (* Player info boxes *)
  let p0buff = GText.buffer ~text: (player_info !current_state 0) () in
  let p0info = GText.view ~editable:false
                          ~packing:(table#attach ~top:1 ~bottom:2
                                                 ~left:2 ~right:5 ~expand:`BOTH)
                          ~buffer:p0buff () in
  let buffers = Array.make 4 p0buff in
  buffers.(1) <- GText.buffer ~text: (player_info !current_state 1) ();
  let p1info = GText.view ~editable:false
                          ~packing:(table#attach ~top:1 ~bottom:2
                                                 ~left:6 ~right:9 ~expand:`BOTH)
                          ~buffer:buffers.(1) () in

  buffers.(2) <- GText.buffer ~text: (player_info !current_state 2) ();
  let p2info = GText.view ~editable:false
                          ~packing:(table#attach ~top:3 ~bottom:4
                                                 ~left:2 ~right:5 ~expand:`BOTH)
                          ~buffer:buffers.(2) () in

  buffers.(3) <- GText.buffer ~text: (player_info !current_state 3) ();
  let p3info = GText.view ~editable:false
                          ~packing:(table#attach ~top:3 ~bottom:4
                                                 ~left:6 ~right:9 ~expand:`BOTH)
                          ~buffer:buffers.(3) () in

  (* [update_info st] clears the info box of all players and fills them with
   * updated information *)
  let update_info st =
    for n = 0 to ((List.length (State.active_players st))-1) do
      buffers.(n)#delete buffers.(n)#start_iter buffers.(n)#end_iter;
      buffers.(n)#insert (player_info st n)
    done;

    if ((List.length (State.active_players st))-1) < 4 then
      for n = ((List.length (State.active_players st))) to 3 do
        buffers.(n)#delete buffers.(n)#start_iter buffers.(n)#end_iter;
        buffers.(n)#insert ""
      done in

  (* General info box *)
  (* CURRENT PLAN: use the general info box for displaying tile info,
   * chance cards, dice rolls, errors, etc.*)
  let gen_buff =
    GText.buffer ~text:("Welcome to Monopoly!"^
                       "\nCurrent turn is "^
                       (let curr_pl_id = State.current_turn !current_state in
                       State.name (get_player_state !current_state curr_pl_id))) () in
  let gen_info = GText.view ~editable:false
                            ~packing:(table#attach ~top:5 ~bottom:7
                                                   ~left:3 ~right:8 ~expand:`BOTH)
                            ~buffer:gen_buff () in

  (*[clear_gen_info ()] clears the general info box *)
  let clear_gen_info () = 
    gen_buff#delete gen_buff#start_iter gen_buff#end_iter in

  (* [tile_info st n] is the displayed information of tile n in state st. *)
  (* TODO: what information to display? *)
  let tile_info st n =
    let tile_state = get_tile_state st n in
    "Location: "^(tile_state.name)^
    "\nSet: "^(string_of_int tile_state.set)^
    "\nCost: "^(string_of_int tile_state.cost)^
    "\nInitial Rent: "^(string_of_int tile_state.initial_rent)^
    "\nOwner: "^(if (tile_state.owner >= 1 && tile_state.owner <= 4) then
                   let owner_id = id_to_index tile_state.owner !current_state in
                   name (get_player_state !current_state owner_id)
                 else
                   "none")^
    "\nNumber of Houses: "^(string_of_int tile_state.curr_house) in

  (* Upon clicking a tile, we first need to check the selling state.
   * If we are not selling anything, we will display tile info in the
   * general info box. If we are selling something, then we will
   * execute the sell command. *)
  for i = 0 to 39
  do
  tiles.(i)#connect#clicked
    ~callback:(fun () -> 
        clear_gen_info ();
        match (!current_sell_state) with
        | No_Sell -> 
          gen_buff#insert (tile_info !current_state i)
        | Mortgage -> 
          (try (current_state := (State.exec (Command.Mortgage i) !current_state);
                let player_state = 
                  get_player_state !current_state (current_turn !current_state) in
                clear_gen_info ();
                gen_buff#insert ((State.name player_state)^" mortgaged "^
                                 (tile_name !current_state i));
                update_info !current_state;
                current_sell_state := No_Sell;
               ) with
          | Invalid_Command str ->
            clear_gen_info ();
            gen_buff#insert (str);
            current_sell_state := No_Sell;
          )
        | Sell -> 
          (try (current_state := (State.exec (Command.Sell_Tile i) !current_state);
                let player_state = 
                  get_player_state !current_state (current_turn !current_state) in
                clear_gen_info ();
                gen_buff#insert ((State.name player_state)^" sold "^
                                 (tile_name !current_state i));
                update_info !current_state;
                current_sell_state := No_Sell;
               ) with
          | Invalid_Command str ->
            clear_gen_info ();
            gen_buff#insert (str);
            current_sell_state := No_Sell;
          )
        | Sell_House -> 
          (try (current_state := (State.exec (Command.Sell_House (1,i)) !current_state);
                let player_state = 
                  get_player_state !current_state (current_turn !current_state) in
                clear_gen_info ();
                gen_buff#insert ((State.name player_state)^" sold a house on "^
                                 (tile_name !current_state i));
                update_info !current_state;
                current_sell_state := No_Sell;
               ) with
          | Invalid_Command str ->
            clear_gen_info ();
            gen_buff#insert (str);
            current_sell_state := No_Sell;
          )
        | Sell_Hotel -> 
          (try (current_state := (State.exec (Command.Sell_Hotel i) !current_state);
                let player_state = 
                  get_player_state !current_state (current_turn !current_state) in
                clear_gen_info ();
                gen_buff#insert ((State.name player_state)^" sold a hotel on "^
                                 (tile_name !current_state i));
                update_info !current_state;
                current_sell_state := No_Sell;
               ) with
          | Invalid_Command str ->
            clear_gen_info ();
            gen_buff#insert (str);
            current_sell_state := No_Sell;
          )
      );
  done;
  tiles.(40)#connect#clicked
    ~callback:(fun () -> clear_gen_info ();
                         match (!current_sell_state) with
                         | No_Sell -> gen_buff#insert "Location: Academic Hearing"
                         | Mortgage -> gen_buff#insert "You can't mortgage this.";
                                       current_sell_state := No_Sell;
                         | Sell -> gen_buff#insert "You can't sell this.";
                                   current_sell_state := No_Sell;
                         | Sell_House -> gen_buff#insert "There is no house here.";
                                         current_sell_state := No_Sell;
                         | Sell_Hotel -> gen_buff#insert "There is no hotel here.";
                                         current_sell_state := No_Sell;
              );

  (* Command buttons *)
  (* TODO: upon clicking a command button, should update current_state, 
   *       and then update the GUI:
   *         -display a message in the general info box describing what happened
   *         -update the player info boxes according to the new state
   *         -update (hide/show) the command buttons according to which 
   *          commands are possible (later)
   *         -update the appearance of the tiles to reflect player 
   *          locations/owned properties (later later)
   *)
  let roll_button = add_button "Roll" 7 2 in
  let roll_widget = new GObj.misc_ops roll_button#as_widget in
  let buy_button = add_button "Buy" 7 3 in
  let buy_widget = new GObj.misc_ops buy_button#as_widget in
  let buy_mort_button = add_button "Buy\nMortgaged" 7 4 in
  let buy_mort_widget = new GObj.misc_ops buy_mort_button#as_widget in
  let buy_house_button = add_button "Buy House" 7 5 in
  let buy_house_widget = new GObj.misc_ops buy_house_button#as_widget in
  let buy_hotel_button = add_button "Buy Hotel" 7 6 in
  let buy_hotel_widget = new GObj.misc_ops buy_hotel_button#as_widget in
  let sell_button = add_button "Sell" 8 3 in
  let sell_widget = new GObj.misc_ops sell_button#as_widget in
  let sell_house_button = add_button "Sell House" 8 5 in
  let sell_house_widget = new GObj.misc_ops sell_house_button#as_widget in
  let sell_hotel_button = add_button "Sell Hotel" 8 6 in
  let sell_hotel_widget = new GObj.misc_ops sell_hotel_button#as_widget in
  let mortgage_button = add_button "Mortgage" 8 4 in
  let mortgage_widget = new GObj.misc_ops mortgage_button#as_widget in
  let inv_button = add_button "Inventory" 7 7 in
  let inv_widget = new GObj.misc_ops inv_button#as_widget in
  let end_button = add_button "End Turn" 7 8 in
  let end_widget = new GObj.misc_ops end_button#as_widget in
  end_widget#hide (); (* invalid until you roll *)
  let quit_button = add_button "Quit" 8 7 in
  let quit_widget = new GObj.misc_ops quit_button#as_widget in

  (* TODO: hide/enable buttons after rolling dice*)
  roll_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := 
                State.exec Command.Turn_start (State.roll_dice !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^
                               " rolled [" ^ 
                               (string_of_int (State.fst_dice !current_state)) ^ 
                               "],[" ^ 
                               (string_of_int (State.snd_dice !current_state)) ^ 
                               "]!");
              if (fst_dice !current_state = snd_dice !current_state) then
                gen_buff#insert ("\nYou rolled a double! You will get another turn.");
              update_info !current_state;
              let new_tile = curr_tile !current_state in
              gen_buff#insert ("\nLanded on "^new_tile.name^".");
              (match (new_tile.tiletype) with
               | "Go" -> ()
               | "Property" -> 
                 (if new_tile.owner = 100 then 
                    gen_buff#insert "\nThis is an unowned property!"
                  else if new_tile.owner = State.curr_turn_id !current_state then 
                    gen_buff#insert "\nYou own this property!"
                  else 
                    gen_buff#insert ("\nPaid rent to "^
                      (let owner_id = (id_to_index new_tile.owner !current_state) in
                      (State.name (State.get_player_state !current_state owner_id)))))
               | "Community_Chest" -> 
                 (gen_buff#insert ("\nDrew a community chest card: "^
                    ((State.get_last_comm_drawn player_state).comm_description)))
               | "Chance" -> 
                 (gen_buff#insert ("\nDrew a chance card: "^
                    ((State.get_last_chance_drawn player_state).ch_description)))
               | "Tax" -> 
                 (gen_buff#insert "\nPaid income tax.")
               | "Train_Station" -> 
                 (if new_tile.owner = 100 then 
                    gen_buff#insert "\nThis is an unowned station!"
                  else if new_tile.owner = State.curr_turn_id !current_state then 
                    gen_buff#insert "\nYou own this station!"
                  else 
                    gen_buff#insert ("\nPaid station fee to "^
                      (let owner_id = (id_to_index new_tile.owner !current_state) in
                      (State.name (State.get_player_state !current_state owner_id)))))
               | "Jail" -> ()
               | "Utilities" -> 
                 (if new_tile.owner = 100 then 
                    gen_buff#insert "\nThis is an unowned utility!"
                  else if new_tile.owner = State.curr_turn_id !current_state then 
                    gen_buff#insert "\nYou own this utility!"
                  else gen_buff#insert ("\nPaid utility fee to "^
                         (let owner_id = (id_to_index new_tile.owner !current_state) in
                         (State.name (State.get_player_state !current_state owner_id)))))
               | "Free_Parking" -> ()
               | "Go_Jail" -> (gen_buff#insert "\nWent to academic hearing!")
               | _ -> failwith "invalid tile type");
              (* update button appearances *)
              roll_widget#hide (); (* can only roll once *)
              end_widget#show (); (* can only end after rolling *)
             ) with
        | Invalid_Command str ->
          clear_gen_info();
          gen_buff#insert (str));
      );

  buy_button#connect#clicked
    ~callback:(fun () ->
        current_sell_state := No_Sell;
        (try (current_state := 
                (State.exec Command.Buy_tile !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^" bought "^
                               (tile_name !current_state (location player_state)));
              update_info !current_state;
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  buy_mort_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := (State.exec Command.Buy_mortgaged !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^" bought "^
                               (tile_name !current_state (location player_state)));
              update_info !current_state;
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  (* For now, can only buy one house at a time; buy multiple houses by clicking
   * the button multiple times *)
  buy_house_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := 
                (State.exec (Command.Buy_houses 1) !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^" bought a house on "^
                               (tile_name !current_state (location player_state)));
              update_info !current_state;
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  buy_hotel_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := (State.exec Command.Buy_hotel !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^" bought a hotel on "^
                               (tile_name !current_state (location player_state)));
              update_info !current_state;
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  inv_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := (State.exec Command.Inventory !current_state);
              let player_state = 
                get_player_state !current_state (State.current_turn !current_state) in
              clear_gen_info ();
              let props = 
                List.map (fun t -> t.name) (State.owned_property player_state) in
              let props_str = 
                List.fold_left (fun a b -> if a <> "" then  a^", "^b else b) "" props in
              gen_buff#insert 
                ((State.name player_state)^"'s Properties: "^props_str);
              gen_buff#insert 
                ("\nClick in the box and use the arrow keys if the line above is cut off.");
              update_info !current_state;
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  quit_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        let player_state = 
          get_player_state !current_state (State.current_turn !current_state) in
        (try (current_state := (State.exec Command.Quit !current_state);
              clear_gen_info ();
              gen_buff#insert ((State.name player_state)^" quit the game! " ^ 
                 (if State.check_winner !current_state then 
                    "Winner is " ^ 
                      (State.name (List.hd (State.active_players !current_state))) 
                  else ""));
              update_info !current_state;
              roll_widget#show (); (* next player can roll now *)
              end_widget#hide () (* next player cannot end turn yet *)
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  end_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := No_Sell;
        (try (current_state := (State.exec Command.EndTurn !current_state);
              clear_gen_info ();
              gen_buff#insert ("Current turn is now "^
                               (State.current_turn !current_state |> 
                                get_player_state !current_state |> 
                                State.name));
              roll_widget#show (); (* next player can roll now *)
              end_widget#hide (); (* next player cannot end turn yet *)
             ) with
        | Invalid_Command str ->
          clear_gen_info ();
          gen_buff#insert (str));
      );

  (* selling/mortgaging is a bit more complicated since the user can
   * sell from anywhere on the board. Our solution is to use the existing 
   * tile buttons to sell the properties. Thus the "sell" logic is 
   * attached to each property button.
   *)
  sell_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := Sell;
        clear_gen_info ();
        (* TODO: hide unowned props *)
        gen_buff#insert (if not (State.game_fin !current_state) then 
                           "Click on the property you want to sell." 
                         else 
                           "Game is finished. You can no longer play.");
      );

  sell_house_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := Sell_House;
        clear_gen_info ();
        (* TODO: hide unowned props *)
        gen_buff#insert (if not (State.game_fin !current_state) then 
                           "Click on the property whose houses you want to sell." 
                         else 
                           "Game is finished. You can no longer play." );
      );

  sell_hotel_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := Sell_Hotel;
        clear_gen_info ();
        (* TODO: hide unowned props *)
        gen_buff#insert (if not (State.game_fin !current_state) then 
                           "Click on the property whose hotel you want to sell."
                         else 
                           "Game is finished. You can no longer play." );
      );

  mortgage_button#connect#clicked
    ~callback:(fun () -> 
        current_sell_state := Mortgage;
        clear_gen_info ();
        (* TODO: hide unowned props *)
        gen_buff#insert (if not (State.game_fin !current_state) then 
                           "Click on the property you want to mortgage." 
                         else 
                           "Game is finished. You can no longer play." );
      );

  (* Display the window and enter Gtk+ main loop *)
  window#show ();
  Main.main ()

let () = main ()
