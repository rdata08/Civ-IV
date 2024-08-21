open Player
open Printf
open Constants

type terrain =
  | Grassland
  | Desert
  | Ocean
  | Mountains
  | Forest

(* by default (unless another country is playing) the structure will be
   empty. *)
(* type structure = | Farm | Road | Harbor | Mine | Empty *)

(* grass -> agriculture desert -> ocean -> fishing *)
type tile = {
  x : int;
  y : int;
  (* mutable structure : structure; *)
  (* by default will be Empty. Players can build stuff here to increase
     output *)
  terrain : terrain;
  mutable country_claimed : string;
  mutable country_built : string;
}

let update_country_claimed_of_tile tile_opt new_country =
  match tile_opt with
  | Some tile -> tile.country_claimed <- new_country
  | None -> ()

let update_country_built_of_tile tile_opt new_country =
  match tile_opt with
  | Some tile -> tile.country_built <- new_country
  | None -> ()

let get_x tile = tile.x
let get_y tile = tile.y
let get_terrain tile = tile.terrain
let get_country_claimed tile = tile.country_claimed
let get_country_built tile = tile.country_built

type map = tile list

let possible_terrains = [ Grassland; Desert; Ocean; Mountains; Forest ]

let compatible_tech (tile : tile) =
  match tile.terrain with
  | Grassland -> "Agriculture"
  | Desert -> "The Wheel"
  | Ocean -> "Fishing"
  | Mountains -> "Mining"
  | Forest -> "Hunting"

let select_random_terrain () =
  let index = Random.int (List.length possible_terrains) in
  List.nth possible_terrains index

let generate_tile (xcoord : int) (ycoord : int) =
  {
    x = xcoord;
    y = ycoord;
    terrain = select_random_terrain ();
    country_claimed = "Unclaimed";
    country_built = "Unbuilt";
  }

(*original width of map, *)
let rec generate_map (width_original : int) (width : int) (height : int) : map =
  match (width, height) with
  | 0, 0 -> [ generate_tile 0 0 ]
  | w, h ->
      if w > 0 then generate_tile w h :: generate_map width_original (w - 1) h
      else
        generate_tile w h :: generate_map width_original width_original (h - 1)

let tile_terrain (terrain : terrain) : string =
  match terrain with
  | Ocean -> "\u{1F30A}"
  | Forest -> "\u{1F332}"
  | Grassland -> "\u{1F33E}"
  | Mountains -> "\u{1F5FB}"
  | Desert -> "\u{1F42A}"

let tile_background_color tile =
  match tile.country_claimed with
  | "Unclaimed" -> 255
  | "English" -> 22
  | "Chinese" -> 202
  | "American" -> 97
  | "French" -> 24
  | "Russian" -> 58
  | _ -> 0

(* changes the tile when it's been built upon. *)
let tile_symbol (tile : tile) : string =
  if tile.country_built = "Unbuilt" then tile_terrain tile.terrain
  else
    match tile.terrain with
    | Grassland -> "\u{1F69C}"
    | Desert -> "\u{1F6E4}"
    | Ocean -> "\u{1F3A3}"
    | Mountains -> "\u{1F3ED}"
    | Forest -> "\u{1FAB5}"
(* | Road -> "\u{1F6E4}" | Harbor -> "\u{1F3A3}" | Mine -> "\u{1F3ED}" | Empty
   -> tile_terrain tile.terrain *)

let rec tile_to_symbol (map : map) (map_width : int) =
  match map with
  | [] -> []
  | h :: t ->
      if h.x = map_width then
        ("n", 0)
        :: (tile_symbol h, tile_background_color h)
        :: tile_to_symbol t map_width
      else
        (tile_symbol h, tile_background_color h) :: tile_to_symbol t map_width

let print_symbol_color (tsymbol : string) (colcode : int) =
  printf "\027[48;5;%dm%s\027[0m" colcode tsymbol

let rec render_map map acc =
  match map with
  | [] -> print_string ""
  | (str, i) :: t ->
      if str = "n" then
        match acc with
        | 10 -> Printf.printf "\n%2d " acc
        | -2 -> Printf.printf "\n%2d " 9
        | -14 -> Printf.printf "\n%2d " 8
        | -26 -> Printf.printf "\n%2d " 7
        | -38 -> Printf.printf "\n%2d " 6
        | -50 -> Printf.printf "\n%2d " 5
        | -62 -> Printf.printf "\n%2d " 4
        | -74 -> Printf.printf "\n%2d " 3
        | -86 -> Printf.printf "\n%2d " 2
        | -98 -> Printf.printf "\n%2d " 1
        | -110 -> Printf.printf "\n%2d " 0
        | _ -> Printf.printf ""
      else print_symbol_color str i;
      render_map t (acc - 1)

type t = {
  civilization : string;
  leader : string;
  starting_tech : string;
  mutable score : int;
  mutable claimed_coordinates : tile list;
  mutable built_coordinates : tile list;
}

let initialize_state civilization leader starting_tech =
  {
    civilization;
    leader;
    starting_tech;
    score = 0;
    claimed_coordinates = [];
    built_coordinates = [];
  }

(* let update_civilization st civ = st.civilization <- civ let update_leader st
   leader = st.leader <- leader let update_starting_tech st starting_tech =
   st.starting_tech <- starting_tech *)
let update_score st delta_score = st.score <- st.score + delta_score

let update_claimed st new_claim =
  match new_claim with
  | Some tile -> st.claimed_coordinates <- tile :: st.claimed_coordinates
  | None -> st.claimed_coordinates <- st.claimed_coordinates

let update_built st new_build =
  match new_build with
  | Some tile -> st.built_coordinates <- tile :: st.built_coordinates
  | None -> st.built_coordinates <- st.built_coordinates

let get_civilization_id st = st.civilization
let get_leader st = st.leader
let get_starting_tech st = st.starting_tech
let get_score st = st.score
let get_claimed_coordinates st = st.claimed_coordinates
let get_built_coordinates st = st.built_coordinates

let get_tile map x_coor y_coor =
  let lst = List.filter (fun tile -> tile.x = x_coor && tile.y = y_coor) map in
  List.nth_opt lst 0

let is_unclaimed map x_coor y_coor =
  let y = List.filter (fun tile -> tile.x = x_coor && tile.y = y_coor) map in
  if y = [] then false else (List.nth y 0).country_claimed = "Unclaimed"

let can_claim map st x y =
  if is_unclaimed map x y then
    let coor_list =
      List.map (fun tile -> (get_x tile, get_y tile)) st.claimed_coordinates
    in
    List.mem (x, y + 1) coor_list
    || List.mem (x, y - 1) coor_list
    || List.mem (x + 1, y) coor_list
    || List.mem (x - 1, y) coor_list
  else false

let can_build st tile_opt =
  let already_built (x : tile option) =
    match x with
    | None -> false
    | Some tile -> get_country_built tile = "Unbuilt"
  in
  if not (already_built tile_opt) then false
  else if get_score st < build_cost then false
  else
    match tile_opt with
    | Some tile ->
        if List.mem tile st.claimed_coordinates then
          match get_terrain tile with
          | Grassland -> st.starting_tech = "Agriculture"
          | Desert -> st.starting_tech = "The Wheel"
          | Ocean -> st.starting_tech = "Fishing"
          | Mountains -> st.starting_tech = "Mining"
          | Forest -> st.starting_tech = "Hunting"
        else false
    | None -> false

let determine_result st computer_score =
  if st.score > computer_score then "You won!"
  else if st.score = computer_score then "You tied!"
  else "You lost!"

let opp (player_civ : Player.t) (str : string) : t =
  let opponent_civ = randomciv player_civ str in
  let opponent_leader = randomleader player_civ opponent_civ in
  let opponent_tech = randomtech player_civ opponent_civ in
  {
    civilization = opponent_civ;
    leader = opponent_leader;
    starting_tech = opponent_tech;
    score = 0;
    claimed_coordinates = [];
    built_coordinates = [];
  }

let status_report st opp =
  print_endline ("Your score: " ^ string_of_int st.score);
  print_endline ("Computer score: " ^ string_of_int opp.score)

let initial_map_maker (st : t) (m : map) (x : int) =
  print_string "   10 9 8 7 6 5 4 3 2 1 0";
  let symbols = tile_to_symbol m x in
  let rendering = render_map symbols 10 in
  rendering;
  print_newline ();
  (* print_string ""; *)
  (* Printf.printf 0; print_string " 0"; for i = 10 to 0 do Printf.printf "%2d"
     i done; *)
  List.iter
    (fun tile ->
      if get_country_claimed tile = get_civilization_id st then
        update_country_claimed_of_tile (Some tile) (get_civilization_id st);
      if List.mem tile (get_built_coordinates st) then
        update_country_built_of_tile (Some tile) (get_civilization_id st))
    m
