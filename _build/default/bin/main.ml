open Civ
open Player
open State
open Turn
open Constants

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

let pp_string_unit s = print_endline s

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  print_endline (pp_elts lst)

let data_dir_prefix = "data" ^ Filename.dir_sep
let civ4 = Yojson.Basic.from_file (data_dir_prefix ^ "civilization.json")
let civ4 = from_json civ4
let chosen_civ = ref ""
let chosen_leader = ref ""

let rec choose_civilization adventure =
  let civ_list = civilization_ids civ4 in
  pp_list pp_string civ_list;
  print_endline "Choose a civilization: ";

  let civilization = read_line () in
  check_for_quit civilization;

  chosen_civ := civilization;

  if List.mem civilization civ_list then ()
  else (
    print_endline "Please enter a valid civilization: ";
    choose_civilization civ4)

let rec choose_leader c =
  let leader_list = leaders civ4 !chosen_civ in
  pp_list pp_string leader_list;
  print_endline "Choose a leader: ";

  let leader = read_line () in
  check_for_quit leader;

  chosen_leader := leader;

  if List.mem leader leader_list then ()
  else (
    print_endline "Please enter a valid leader: ";
    choose_leader !chosen_civ)

let rec choose_start_tech c =
  let start_tech_list = starting_tech civ4 !chosen_civ in
  pp_list pp_string start_tech_list;
  print_endline "Choose a starting technology: ";

  let starting_tech = read_line () in
  check_for_quit starting_tech;

  if List.mem starting_tech start_tech_list then
    initialize_state !chosen_civ !chosen_leader starting_tech
  else (
    print_endline "Please enter a valid starting technology: ";
    choose_start_tech !chosen_civ)

let main () =
  print_endline "Welcome! Please enter your name: ";
  let name = read_line () in
  check_for_quit name;
  print_endline ("Hello " ^ name ^ "!");

  choose_civilization civ4;
  let unit = unique_unit civ4 !chosen_civ in
  let building = unique_building civ4 !chosen_civ in

  print_endline ("Unit " ^ unit ^ " acquired. Building " ^ building ^ " acquired.");

  choose_leader !chosen_civ;

  let state = choose_start_tech !chosen_civ in

  let opp_state = opp civ4 !chosen_civ in

  (* just for compilation *)
  update_score state 0;

  print_endline ("Good luck.");

  (* print_endline ("Your Score: " ); print_endline ("Computer Score: "); *)
  let first_map = generate_map 10 10 10 in

  (* initial_map_maker state first_map 10; *)
  let rec take_turns round =
    if round > total_round then ()
    else (
      print_endline ("Round: " ^ string_of_int round);
      player_turn state first_map;
      status_report state opp_state;
      (* initial_map_maker state first_map 10; initial_map_maker opp_state
         first_map 10; *)
      print_endline ("Round: " ^ string_of_int (round + 1));
      opponent_turn opp_state first_map map_size;
      status_report state opp_state;
      initial_map_maker opp_state first_map 10;
      take_turns (round + 2))
  in

  take_turns 0;
  (* initial_map_maker opp_state first_map 10; initial_map_maker state first_map
     10; *)
  print_endline (determine_result state (get_score opp_state))

(* Execute the game engine. *)
let () = main ()
