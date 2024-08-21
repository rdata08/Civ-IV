type t
(** The abstract type of values representing game states. *)

(* type structure *)
(** The type representing the structure built on the tile. Set to empty by
    default. *)

type tile
(** The type [tile] represents a tile on the map that is decomposed into a
    x-coordinate, y-coordinate, mutable structure, terrain, mutable
    country_claimed, and mutable country_built. *)

type terrain
(** The type representing the terrain of a tile. *)

type map
(** The type representing the map of the adventure. *)

val initialize_state : string -> string -> string -> t
(** [initialize_state id leader tech] returns a new game state t with the
   specified civilization id, with leader name leader, and starting technology
   tech. The mutable field score is set to 0 and the mutable fields
   claimed_coordinates and built_coordinates are set to an empty list. *)

val opp : Player.t -> string -> t
(** [opp player c] initializes an opponent with a random civilization, leader,
   and starting technology. The opponent's civilization is chosen randomly from
   the available civilizations excluding c. The opponent starts with a score of
   0 and empty lists for claimed and built coordinates. *)

val update_score : t -> int -> unit
(** [update_score st score] updates the score of the game's state st with the
   provided score.*)

val update_claimed : t -> tile option -> unit
(** [update_claimed st t] updates the game's state st to reflect the claimed
   status of the provided tile t *)

val update_built : t -> tile option -> unit
(** [update_built st t] updates the game state st to reflect that a structure has
   been built on the provided tile t *)

val get_civilization_id : t -> string
(** [get_civilization_id st] returns the civilization id associated with the
   player in the game state st *)

val get_country_claimed : tile -> string
(** [get_country_claimed t] returns the string of a country name which has a
   claim on the input tile. *)

val get_country_built : tile -> string
(** [get_country_built t] returns the string of a country name which has built on
   the input tile. *)

val get_leader : t -> string
(** [get_leader st] returns the leader name associated with the player in the
   game state st. *)

val get_starting_tech : t -> string
(* [get_starting_tech st] returns the starting technology associated with the
   player in the game state st. *)

val get_score : t -> int
(** [get_score st] returns the score associated with the player in the game state
   st.*)

val get_claimed_coordinates : t -> tile list
(** [get_claimed_coordinates st] returns a list of tiles that the player has
   claimed in the game state st *)

val get_built_coordinates : t -> tile list
(** [get_built_coordinates st] returns a list of tiles where the player has built
   structures in the game state st.*)

val can_build : t -> tile option -> bool
(** [can_build st t] returns true if the player is allowed to build structures on
   the specified tile t in the game state st, and false otherwise.*)

val determine_result : t -> int -> string
(** [determine_result st cs] determines the result of the game in the game state
   st by comparing the player's score with the computer score cs. The result is
   returned as a value of type result*)

val update_country_claimed_of_tile : tile option -> string -> unit
(** [update_country_claimed_of_tile t_opt new_country] updates the
   country_claimed component of tile with the name of new_country that has
   claimed the tile. *)

val update_country_built_of_tile : tile option -> string -> unit
(** [update_country_built_of_tile t_opt new_country] updates the country_claimed
   component of tile with the name of new_country that has built on the tile. *)

val get_x : tile -> int
(** Returns the x-coordinate of the given tile*)

val get_y : tile -> int
(** Returns the y-coordinate of the given tile*)

val get_terrain : tile -> terrain
(** Returns the terrain of the tile*)

val compatible_tech : tile -> string
(** Returns the corresponding technology for the terrain of the input tile.*)

val generate_tile : int -> int -> tile
(** Returns a tile for input co-ordinates with a random terrain*)

val generate_map : int -> int -> int -> map
(** [generate_map o w h] returns a map of tiles based on the original width o,
  width w, and height h. *)

val tile_terrain : terrain -> string
(*[tile_terrain t] returns a string representation of the emoji associated with
  the given terrain.*)

val tile_symbol : tile -> string
(** [tile_symbol t] returns a string representation of the symbol associated with
   tile's structure and terrain status. Updates tile if a structure has been
   built upon it.*)

val tile_to_symbol : map -> int -> (string * int) list
(** [tile_to_symbol m x] returns returns a list of strings representing the
  symbols of tiles in the map m *)

val tile_background_color : tile -> int
(** [tile_background_color tile] matches country claim on a tile to a background
   color. *)

val print_symbol_color : string -> int -> unit
(** [print_symbol_color str colorcode] generates a tile with their symbol and
   background color in a format readable to the renderer function.*)

val render_map : (string * int) list -> int -> unit
(** [render_map sl] prints the strings in sl to the console, effectively rendering
  a map.*)

val initial_map_maker : t -> map -> int -> unit
(** Renders a map, printing out on the terminal *)

val get_tile : map -> int -> int -> tile option
(** [get_tile map x_coor y_coor] returns the tile in map that is located at
   (x_coor, y_coor). *)

val is_unclaimed : map -> int -> int -> bool
(** [is_unclaimed map x_coor y_coor] returns true if the tile located at (x_coor,
   y_coor) is unclaimed, and false otherwise. *)

val can_claim : map -> t -> int -> int -> bool
(** [can_build map x y] returns true if the player is allowed to claim the
   specified tile located on (x, y) of map, and false otherwise.*)

val select_random_terrain : unit -> terrain
(** Returns a randomly selected terrain. *)

val status_report : t -> t -> unit
(** [status_report st opp] prints out your score and computer score in
    consecutive lines. *)
