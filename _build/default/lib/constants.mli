(** Total number of rounds of a single game. *)
val total_round : int

(** The size of the square map. *)
val map_size : int

(** The amount of points gained from claiming a tile. *)
val claim_reward : int

(** The amount of points gained per round after building on a tile. *)
val build_reward : int

(** The cost to build on a tile. *)
val build_cost : int
