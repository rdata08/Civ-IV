open Yojson.Basic.Util

exception UnknownCivilization of string

type leader = { name : string }

type starting_tech = {
  name : string;
  cost : int;
}

type civilization = {
  id : string;
  leaders : leader list;
  starting_tech : starting_tech list;
  unique_unit : string;
  unique_building : string;
}

type t = { civilizations : civilization list }

let leader_of_json j = { name = j |> member "name" |> to_string }

let starting_tech_of_json j =
  {
    name = j |> member "name" |> to_string;
    cost = j |> member "cost" |> to_int;
  }

let civilization_of_json j =
  {
    id = j |> member "id" |> to_string;
    leaders = j |> member "leaders" |> to_list |> List.map leader_of_json;
    starting_tech =
      j |> member "starting tech" |> to_list |> List.map starting_tech_of_json;
    unique_unit = j |> member "unique unit" |> to_string;
    unique_building = j |> member "unique building" |> to_string;
  }

let t_of_json j =
  {
    civilizations =
      j |> member "civilizations" |> to_list |> List.map civilization_of_json;
  }

let parse j =
  try t_of_json j with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let from_json json = parse json

let rec civilization_ids_helper civilization_list =
  match civilization_list with
  | [] -> []
  | h :: t -> h.id :: civilization_ids_helper t

let civilization_ids adv =
  List.sort_uniq compare (civilization_ids_helper adv.civilizations)

let rec search_for_civilization civilization_list civilization =
  match civilization_list with
  | [] -> false
  | h :: t ->
      if h.id = civilization then true
      else search_for_civilization t civilization

let rec make_leader_list civilization_list civilization =
  match civilization_list with
  | [] -> []
  | h :: t ->
      if h.id = civilization then h.leaders else make_leader_list t civilization

let rec leader_list_name (leader_list : leader list) : string list =
  match leader_list with
  | [] -> []
  | h :: t -> h.name :: leader_list_name t

let leaders adv civilization =
  if search_for_civilization adv.civilizations civilization then
    civilization
    |> make_leader_list adv.civilizations
    |> leader_list_name |> List.sort_uniq compare
  else raise (UnknownCivilization civilization)

let rec make_starting_tech_list civilization_list civilization =
  match civilization_list with
  | [] -> []
  | h :: t ->
      if h.id = civilization then h.starting_tech
      else make_starting_tech_list t civilization

let rec starting_tech_list_name (starting_tech_list : starting_tech list) :
    string list =
  match starting_tech_list with
  | [] -> []
  | h :: t -> h.name :: starting_tech_list_name t

let starting_tech adv civilization =
  if search_for_civilization adv.civilizations civilization then
    civilization
    |> make_starting_tech_list adv.civilizations
    |> starting_tech_list_name |> List.sort_uniq compare
  else raise (UnknownCivilization civilization)

let rec unique_unit_helper civilization_list civilization =
  match civilization_list with
  | [] -> ""
  | h :: t ->
      if h.id = civilization then h.unique_unit
      else unique_unit_helper t civilization

let unique_unit adv civilization =
  if search_for_civilization adv.civilizations civilization then
    unique_unit_helper adv.civilizations civilization
  else raise (UnknownCivilization civilization)

let rec unique_building_helper civilization_list civilization =
  match civilization_list with
  | [] -> ""
  | h :: t ->
      if h.id = civilization then h.unique_building
      else unique_building_helper t civilization

let unique_building adv civilization =
  if search_for_civilization adv.civilizations civilization then
    unique_building_helper adv.civilizations civilization
  else raise (UnknownCivilization civilization)

let randomciv (t : t) (picked : string) =
  let civs = civilization_ids t in
  let available_civs = List.filter (fun civ -> not (civ = picked)) civs in
  let random_index = Random.int (List.length available_civs) in
  List.nth available_civs random_index

let randomleader (t : t) (civ : string) =
  let leaders = leaders t civ in
  let random_index = Random.int (List.length leaders) in
  List.nth leaders random_index

let randomtech (t : t) (civ : string) =
  let techs = starting_tech t civ in
  let random_index = Random.int (List.length techs) in
  List.nth techs random_index
