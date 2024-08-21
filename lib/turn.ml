open Player
open State
open Constants

let next_val =
  let round = ref 0 in
  fun () ->
    incr round;
    !round

let check_for_quit input =
  if String.trim (String.lowercase_ascii input) = "quit" then exit 0 else ()

let rec from i j l = if i > j then l else from i (j - 1) (string_of_int j :: l)
let ( -- ) i j = from i j []
let allowed_coor = 0 -- map_size

let rec enter_xcoor msg =
  print_endline msg;
  let x_coor = read_line () in
  check_for_quit x_coor;
  let x_coor = String.trim x_coor in
  if not (List.mem x_coor allowed_coor) then (
    print_endline "Invalid x coordinate";
    enter_xcoor msg)
  else int_of_string x_coor

let rec enter_ycoor msg =
  print_endline msg;
  let y_coor = read_line () in
  check_for_quit y_coor;
  let y_coor = String.trim y_coor in
  if not (List.mem y_coor allowed_coor) then (
    print_endline "Invalid y coordinate";
    enter_ycoor msg)
  else int_of_string y_coor

let rec claim_build_skip (st : State.t) (map : State.map) : unit =
  print_endline "Enter command: (Claim / Build / Skip)";
  let command = read_line () in
  check_for_quit command;
  let command = String.lowercase_ascii command in
  match command with
  | "claim" -> claim_exe st map
  | "build" -> build_exe st map
  | "skip" -> ()
  | _ -> claim_build_skip st map

and claim_exe st map =
  let x_coor = enter_xcoor "Enter x coordinate: " in
  let y_coor = enter_ycoor "Enter y coordinate: " in

  if can_claim map st x_coor y_coor then (
    let tile = get_tile map x_coor y_coor in
    update_claimed st tile;
    update_country_claimed_of_tile tile (get_civilization_id st);
    update_score st claim_reward;
    print_endline
      ("You have claimed tile (" ^ string_of_int x_coor ^ ", "
     ^ string_of_int y_coor ^ ")!"))
  else (
    print_endline "Invalid coordinates";
    claim_build_skip st map)

and build_exe st map =
  let x_coor = enter_xcoor "Enter x coordinate: " in
  let y_coor = enter_ycoor "Enter y coordinate: " in
  let tile = get_tile map x_coor y_coor in
  if can_build st tile then (
    update_built st tile;
    update_score st (-build_cost);
    update_country_built_of_tile tile (get_civilization_id st);
    print_endline
      ("You have built on tile (" ^ string_of_int x_coor ^ ", "
     ^ string_of_int y_coor ^ ")!"))
  else (
    print_endline "Invalid coordinates";
    claim_build_skip st map)

let player_turn (st : State.t) (map : State.map) : unit =
  if get_claimed_coordinates st = [] then (
    print_endline "You have claimed tile (0, 0)!";
    update_claimed st (get_tile map 0 0);
    update_country_claimed_of_tile (get_tile map 0 0) (get_civilization_id st);
    update_score st claim_reward)
  else (
    (*update score before choice*)
    update_score st (build_reward * List.length (get_built_coordinates st));
    (*Player makes choice. Update state accordingly*) claim_build_skip st map)

let opponent_turn (opp_state : State.t) (map : State.map) (map_size : int) :
    unit =
  if get_claimed_coordinates opp_state = [] then (
    print_endline
      ("Opponent has claimed tile (" ^ string_of_int map_size ^ ", "
     ^ string_of_int map_size ^ ")!");
    update_claimed opp_state (get_tile map map_size map_size);
    update_country_claimed_of_tile
      (get_tile map map_size map_size)
      (get_civilization_id opp_state);
    update_score opp_state claim_reward)
  else (
    (*update score before claim or build*)
    update_score opp_state
      (build_reward * List.length (get_built_coordinates opp_state));

    (*check for build condition*)
    (*1. check if opp score is sufficient*)
    let score_sufficient = build_cost <= get_score opp_state in

    (*2. check if opp has the right tech*)
    let claimed_opt_list =
      List.map (fun v -> Some v) (get_claimed_coordinates opp_state)
    in
    let can_build_lst =
      List.filter (fun tile -> can_build opp_state tile) claimed_opt_list
    in
    let tech_sufficient = not (can_build_lst = []) in
    if score_sufficient && tech_sufficient then (
      (*build*)
      let index = Random.int (List.length can_build_lst) in
      let tile_opt = List.nth can_build_lst index in
      update_built opp_state tile_opt;
      update_score opp_state (-build_cost);
      update_country_built_of_tile tile_opt (get_civilization_id opp_state);

      print_endline
        ("Opponent has built on tile ("
        ^ string_of_int (get_x (Option.get tile_opt))
        ^ ", "
        ^ string_of_int (get_y (Option.get tile_opt))
        ^ ")!"))
    else
      (* Claim randomly *)
      let up_lst =
        List.filter_map
          (fun tile -> get_tile map (get_x tile) (get_y tile + 1))
          (get_claimed_coordinates opp_state)
      in
      let down_lst =
        List.filter_map
          (fun tile -> get_tile map (get_x tile) (get_y tile - 1))
          (get_claimed_coordinates opp_state)
      in
      let left_lst =
        List.filter_map
          (fun tile -> get_tile map (get_x tile - 1) (get_y tile))
          (get_claimed_coordinates opp_state)
      in
      let right_lst =
        List.filter_map
          (fun tile -> get_tile map (get_x tile + 1) (get_y tile))
          (get_built_coordinates opp_state)
      in

      let claimable_list =
        List.sort_uniq compare (up_lst @ down_lst @ left_lst @ right_lst)
      in
      let claimable_list =
        List.filter
          (fun tile -> get_country_claimed tile = "Unclaimed")
          claimable_list
      in
      let index = Random.int (List.length claimable_list) in
      let tile = List.nth claimable_list index in
      update_claimed opp_state (Some tile);
      update_country_claimed_of_tile (Some tile) (get_civilization_id opp_state);
      update_score opp_state claim_reward;
      print_endline
        ("Opponent has claimed tile ("
        ^ string_of_int (get_x tile)
        ^ ", "
        ^ string_of_int (get_y tile)
        ^ ")!"))
