val next_val : unit -> int
(** Increments round by one every time it is called. *)

val check_for_quit : string -> unit
(** If user input is "quit", terminate game. Do nothing otherwise. *)

val claim_build_skip : State.t -> State.map -> unit
(** Based on the player's command, print out instructions for claiming,
    building, or skipping for that round. Update corresponding fields after a
    claim or a build has taken place. *)

val player_turn : State.t -> State.map -> unit
(** If the game is at round 1, let player claim tile (0, 0) and update score by
    claim_reward. If not, start by updating player's score by build_reward * the
    number of structures the player has built in the past rounds. Once the score
    has been updated, prompt player with claim, build, or skip. Update player's
    fields according to the input commands. *)

val opponent_turn : State.t -> State.map -> int -> unit
(** If the game is at round 2, let computer opponent claim tile (n, n), where n
    is the map size. Then update the computer's score by claim_reward. If not,
    start by updating the opponent's score by build_reward * the number of
    structures the opponent has built in the past rounds. Then check whether or
    not the opponent can build. The opponent can build iff their score is >=
    cost_build and if their starting tech corresponds to the terrain of the tile
    that they can build on. If the opponent can build, randomly select the tile
    that they can build on and build. If not, randomly select the tile to claim.
    Update opponent's fields accordingly. *)
