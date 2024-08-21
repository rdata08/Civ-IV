type t
(** The abstract type of values representing adventures. *)

exception UnknownCivilization of string
(** Raised when an unknown civilization identifier is encountered. It carries
    the identifier of the unknown civilization. *)

type leader
(** The type representing the leader of a civilization. *)

type starting_tech
(** The type representing the starting technology of a civilization. *)

type civilization
(** The type representing civilization. *)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the adventure that [j] represents. Requires: [j] is a valid
    JSON adventure representation. *)

val civilization_ids : t -> string list
(** [civilization_ids a] is a set-like list of all of the civilization
    identifiers in adventure [a]. *)

val leaders : t -> string -> string list
(** [leaders a r] is a set-like list of all leader names from the civilization
    with identifier [r] in adventure [a]. Raises [UnknownCivilization r] if [r]
    is not a civilization identifier in [a]. *)

val starting_tech : t -> string -> string list
(** [starting_tech a r] is a set-like list of all starting_tech from the
    civilization with identifier [r] in adventure [a]. Raises
    [UnknownCivilization r] if [r] is not a civilization identifier in [a]. *)

val unique_unit : t -> string -> string
(** [unique_unit a r] is the unique_unit from the civilization with identifier
    [r] in adventure [a]. Raises [UnknownCivilization r] if [r] is not a
    civilization identifier in [a]. *)

val unique_building : t -> string -> string
(** [unique_building a r] is the unique_building from the civilization with
    identifier [r] in adventure [a]. Raises [UnknownCivilization r] if [r] is
    not a civilization identifier in [a]. *)

val randomciv : t -> string -> string
(** [randomciv t civ] generates a random civilization from a list of
   civilizations that excludes civ. *)

val randomleader : t -> string -> string
(** [randomleader t civ] generates a random leader from the list of leaders of
   civ. *)

val randomtech : t -> string -> string
(** [randomtech t civ] generates a random tech from the list of techs of civ. *)
