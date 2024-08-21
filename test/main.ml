open OUnit2
open Civ
open Player
open State
open Turn

(* Test Plan: We tested functions automatically by OUnit across our state 
and player modules. Test cases were developed using both glass box and 
black box testing. We started with black box testing functions to evaluate 
if our functions returned the appropriate result that we were looking for. 
We utilized black box testing throughout our Test Driven Development until 
we finished the game. We omitted testing printed prompts in turn.ml and 
functions that generated random civilizations. This is because the 
functions in turn.ml is dependent on the functionality of the rest of the 
modules in our project and generating randomness for different types in the 
game is for the opponent, which are both invisible and unimportant for our 
player in Civ4. Once we finished our game, we utilized glass box testing 
to evaluate branches in our functions that were not examined as our game 
progressed. This procedure for testing asserts that exceptions are raised 
when invalid prompts are given to functions. This test plan demonstrates 
the correctness of our system because it evaluates branches of functions 
essential to both the game’s functionality and the user.
*)

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

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
  "[" ^ pp_elts lst ^ "]"

(********************************************************************
   End helper functions.
 ********************************************************************)

(*************************** Player tests *******************************)
let data_dir_prefix = "data" ^ Filename.dir_sep
let game = Yojson.Basic.from_file (data_dir_prefix ^ "civilization.json")
let game2 = Yojson.Basic.from_file (data_dir_prefix ^ "civilization2.json")

(* You should not be testing any helper functions here. Test only the functions
   exposed in the [.mli] files. Do not expose your helper functions. See the
   handout for an explanation. *)

(* TODO: add unit tests for modules below. You are free to reorganize the
   definitions below. Just keep it clear which tests are for which modules. *)

let game = from_json game
let game2 = from_json game2

let civilization_ids_test name adv expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (civilization_ids adv)

let civilization_ids_tests =
  [
    civilization_ids_test "All available civilizations of game" game
      [ "American"; "Chinese"; "English"; "French"; "Russian" ];
    civilization_ids_test "All available civilizations of game2" game2
      [ "Arabian"; "Aztec"; "Babylonian"; "Byzantine"; "Carthaginian" ];
  ]

let leaders_test name adv civ expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (leaders adv civ)

let american_leaders = [ "Lincoln"; "Roosevelt"; "Washington" ]
let chinese_leaders = [ "Mao Zedong"; "Qin Shi Huang" ]
let english_leaders = [ "Elizabeth"; "Churchill"; "Victoria" ]
let french_leaders = [ "De Gaulle"; "Louis XIV"; "Napoleon" ]
let russian_leaders = [ "Peter"; "Catherine"; "Stalin" ]
let arabian_leaders = [ "Saladin" ]
let aztec_leaders = [ "Montezuma" ]
let babylonian_leaders = [ "Hammurabi" ]
let byzantine_leaders = [ "Justinian I" ]
let carthaginian_leaders = [ "Hannibal" ]

let leaders_tests =
  [
    leaders_test "American leaders" game "American" american_leaders;
    leaders_test "Chinese leaders" game "Chinese" chinese_leaders;
    leaders_test "English leaders" game "English" english_leaders;
    leaders_test "French leaders" game "French" french_leaders;
    leaders_test "Russian leaders" game "Russian" russian_leaders;
    leaders_test "Arabian leaders" game2 "Arabian" arabian_leaders;
    leaders_test "Aztec leaders" game2 "Aztec" aztec_leaders;
    leaders_test "Babylonian leaders" game2 "Babylonian" babylonian_leaders;
    leaders_test "Byzantine leaders" game2 "Byzantine" byzantine_leaders;
    leaders_test "Carthaginian leaders" game2 "Carthaginian"
      carthaginian_leaders;
  ]

let leaders_exception_test name adv civ expected_output =
  name >:: fun _ -> assert_raises expected_output (fun () -> leaders adv civ)

let leaders_exception_tests =
  [
    leaders_exception_test "Arabian leaders" game "Arabian"
      (UnknownCivilization "Arabian");
    leaders_exception_test "Aztec leaders" game "Aztec"
      (UnknownCivilization "Aztec");
    leaders_exception_test "Babylonian leaders" game "Babylonian"
      (UnknownCivilization "Babylonian");
    leaders_exception_test "Byzantine leaders" game "Byzantine"
      (UnknownCivilization "Byzantine");
    leaders_exception_test "Carthaginian leaders" game "Carthaginian"
      (UnknownCivilization "Carthaginian");
    leaders_exception_test "Celtic leaders" game "Celtic"
      (UnknownCivilization "Celtic");
    leaders_exception_test "Dutch leaders" game "Dutch"
      (UnknownCivilization "Dutch");
    leaders_exception_test "Egyptian leaders" game "Egyptian"
      (UnknownCivilization "Egyptian");
    leaders_exception_test "Ethiopian leaders" game "Ethiopian"
      (UnknownCivilization "Ethiopian");
    leaders_exception_test "German leaders" game "German"
      (UnknownCivilization "German");
    leaders_exception_test "Greek leaders" game "Greek"
      (UnknownCivilization "Greek");
    leaders_exception_test "Holy Roman leaders" game "Holy Roman"
      (UnknownCivilization "Holy Roman");
    leaders_exception_test "Incan leaders" game "Incan"
      (UnknownCivilization "Incan");
    leaders_exception_test "Indian leaders" game "Indian"
      (UnknownCivilization "Indian");
    leaders_exception_test "Japanese leaders" game "Japanese"
      (UnknownCivilization "Japanese");
    leaders_exception_test "Khmer leaders" game "Khmer"
      (UnknownCivilization "Khmer");
    leaders_exception_test "Korean leaders" game "Korean"
      (UnknownCivilization "Korean");
    leaders_exception_test "Malinese leaders" game "Malinese"
      (UnknownCivilization "Malinese");
    leaders_exception_test "Mayan leaders" game "Mayan"
      (UnknownCivilization "Mayan");
    leaders_exception_test "Mongolian leaders" game "Mongolian"
      (UnknownCivilization "Mongolian");
    leaders_exception_test "Native American leaders" game "Native American"
      (UnknownCivilization "Native American");
    leaders_exception_test "Ottoman leaders" game "Ottoman"
      (UnknownCivilization "Ottoman");
    leaders_exception_test "Persian leaders" game "Persian"
      (UnknownCivilization "Persian");
    leaders_exception_test "Portuguese leaders" game "Portuguese"
      (UnknownCivilization "Portuguese");
    leaders_exception_test "Roman leaders" game "Roman"
      (UnknownCivilization "Roman");
    leaders_exception_test "Spanish leaders" game "Spanish"
      (UnknownCivilization "Spanish");
    leaders_exception_test "Sumerian leaders" game "Sumerian"
      (UnknownCivilization "Sumerian");
    leaders_exception_test "Viking leaders" game "Viking"
      (UnknownCivilization "Viking");
    leaders_exception_test "Zulu leaders" game "Zulu"
      (UnknownCivilization "Zulu");
    leaders_exception_test "American leaders" game2 "American"
      (UnknownCivilization "American");
    leaders_exception_test "Chinese leaders" game2 "Chinese"
      (UnknownCivilization "Chinese");
    leaders_exception_test "English leaders" game2 "English"
      (UnknownCivilization "English");
    leaders_exception_test "French leaders" game2 "French"
      (UnknownCivilization "French");
    leaders_exception_test "Russian leaders" game2 "Russian"
      (UnknownCivilization "Russian");
    leaders_exception_test "Celtic leaders" game2 "Celtic"
      (UnknownCivilization "Celtic");
    leaders_exception_test "Dutch leaders" game2 "Dutch"
      (UnknownCivilization "Dutch");
    leaders_exception_test "Egyptian leaders" game2 "Egyptian"
      (UnknownCivilization "Egyptian");
    leaders_exception_test "Ethiopian leaders" game2 "Ethiopian"
      (UnknownCivilization "Ethiopian");
    leaders_exception_test "German leaders" game2 "German"
      (UnknownCivilization "German");
    leaders_exception_test "Greek leaders" game2 "Greek"
      (UnknownCivilization "Greek");
    leaders_exception_test "Holy Roman leaders" game2 "Holy Roman"
      (UnknownCivilization "Holy Roman");
    leaders_exception_test "Incan leaders" game2 "Incan"
      (UnknownCivilization "Incan");
    leaders_exception_test "Indian leaders" game2 "Indian"
      (UnknownCivilization "Indian");
    leaders_exception_test "Japanese leaders" game2 "Japanese"
      (UnknownCivilization "Japanese");
    leaders_exception_test "Khmer leaders" game2 "Khmer"
      (UnknownCivilization "Khmer");
    leaders_exception_test "Korean leaders" game2 "Korean"
      (UnknownCivilization "Korean");
    leaders_exception_test "Malinese leaders" game2 "Malinese"
      (UnknownCivilization "Malinese");
    leaders_exception_test "Mayan leaders" game2 "Mayan"
      (UnknownCivilization "Mayan");
    leaders_exception_test "Mongolian leaders" game2 "Mongolian"
      (UnknownCivilization "Mongolian");
    leaders_exception_test "Native American leaders" game2 "Native American"
      (UnknownCivilization "Native American");
    leaders_exception_test "Ottoman leaders" game2 "Ottoman"
      (UnknownCivilization "Ottoman");
    leaders_exception_test "Persian leaders" game2 "Persian"
      (UnknownCivilization "Persian");
    leaders_exception_test "Portuguese leaders" game2 "Portuguese"
      (UnknownCivilization "Portuguese");
    leaders_exception_test "Roman leaders" game2 "Roman"
      (UnknownCivilization "Roman");
    leaders_exception_test "Spanish leaders" game2 "Spanish"
      (UnknownCivilization "Spanish");
    leaders_exception_test "Sumerian leaders" game2 "Sumerian"
      (UnknownCivilization "Sumerian");
    leaders_exception_test "Viking leaders" game2 "Viking"
      (UnknownCivilization "Viking");
    leaders_exception_test "Zulu leaders" game2 "Zulu"
      (UnknownCivilization "Zulu");
  ]

let starting_tech_test name adv civ expected_output =
  name >:: fun _ ->
  assert_equal ~cmp:cmp_set_like_lists ~printer:(pp_list pp_string)
    expected_output (starting_tech adv civ)

let american_starting_tech = [ "Agriculture"; "Fishing" ]
let chinese_starting_tech = [ "Agriculture"; "Mining" ]
let english_starting_tech = [ "Fishing"; "Mining" ]
let french_starting_tech = [ "Agriculture"; "The Wheel" ]
let russian_starting_tech = [ "Hunting"; "Mining" ]
let arabian_starting_tech = [ "Mysticism"; "The Wheel" ]
let aztec_starting_tech = [ "Hunting"; "Mysticism" ]
let babylonian_starting_tech = [ "Agriculture"; "The Wheel" ]
let byzantine_starting_tech = [ "Mysticism"; "The Wheel" ]
let carthaginian_starting_tech = [ "Fishing"; "Mining" ]

let starting_tech_exception_test name adv civ expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> starting_tech adv civ)

let starting_tech_tests =
  [
    starting_tech_test "American starting tech" game "American"
      american_starting_tech;
    starting_tech_test "Chinese starting tech" game "Chinese"
      chinese_starting_tech;
    starting_tech_test "English starting tech" game "English"
      english_starting_tech;
    starting_tech_test "French starting tech" game "French" french_starting_tech;
    starting_tech_test "Russian starting tech" game "Russian"
      russian_starting_tech;
    starting_tech_test "Arabian starting tech" game2 "Arabian"
      arabian_starting_tech;
    starting_tech_test "Aztec starting tech" game2 "Aztec" aztec_starting_tech;
    starting_tech_test "Babylonian starting tech" game2 "Babylonian"
      babylonian_starting_tech;
    starting_tech_test "Byzantine starting tech" game2 "Byzantine"
      byzantine_starting_tech;
    starting_tech_test "Carthaginian starting tech" game2 "Carthaginian"
      carthaginian_starting_tech;
  ]

let starting_tech_exception_tests =
  [
    starting_tech_exception_test "Arabian starting tech" game "Arabian"
      (UnknownCivilization "Arabian");
    starting_tech_exception_test "Aztec starting tech" game "Aztec"
      (UnknownCivilization "Aztec");
    starting_tech_exception_test "Babylonian starting tech" game "Babylonian"
      (UnknownCivilization "Babylonian");
    starting_tech_exception_test "Byzantine starting tech" game "Byzantine"
      (UnknownCivilization "Byzantine");
    starting_tech_exception_test "Carthaginian starting tech" game
      "Carthaginian" (UnknownCivilization "Carthaginian");
    starting_tech_exception_test "Celtic starting tech" game "Celtic"
      (UnknownCivilization "Celtic");
    starting_tech_exception_test "Dutch starting tech" game "Dutch"
      (UnknownCivilization "Dutch");
    starting_tech_exception_test "Egyptian starting tech" game "Egyptian"
      (UnknownCivilization "Egyptian");
    starting_tech_exception_test "Ethiopian starting tech" game "Ethiopian"
      (UnknownCivilization "Ethiopian");
    starting_tech_exception_test "German starting tech" game "German"
      (UnknownCivilization "German");
    starting_tech_exception_test "Greek starting tech" game "Greek"
      (UnknownCivilization "Greek");
    starting_tech_exception_test "Holy Roman starting tech" game "Holy Roman"
      (UnknownCivilization "Holy Roman");
    starting_tech_exception_test "Incan starting tech" game "Incan"
      (UnknownCivilization "Incan");
    starting_tech_exception_test "Indian starting tech" game "Indian"
      (UnknownCivilization "Indian");
    starting_tech_exception_test "Japanese starting tech" game "Japanese"
      (UnknownCivilization "Japanese");
    starting_tech_exception_test "Khmer starting tech" game "Khmer"
      (UnknownCivilization "Khmer");
    starting_tech_exception_test "Korean starting tech" game "Korean"
      (UnknownCivilization "Korean");
    starting_tech_exception_test "Malinese starting tech" game "Malinese"
      (UnknownCivilization "Malinese");
    starting_tech_exception_test "Mayan starting tech" game "Mayan"
      (UnknownCivilization "Mayan");
    starting_tech_exception_test "Mongolian starting tech" game "Mongolian"
      (UnknownCivilization "Mongolian");
    starting_tech_exception_test "Native American starting tech" game
      "Native American" (UnknownCivilization "Native American");
    starting_tech_exception_test "Ottoman starting tech" game "Ottoman"
      (UnknownCivilization "Ottoman");
    starting_tech_exception_test "Persian starting tech" game "Persian"
      (UnknownCivilization "Persian");
    starting_tech_exception_test "Portuguese starting tech" game "Portuguese"
      (UnknownCivilization "Portuguese");
    starting_tech_exception_test "Roman starting tech" game "Roman"
      (UnknownCivilization "Roman");
    starting_tech_exception_test "Spanish starting tech" game "Spanish"
      (UnknownCivilization "Spanish");
    starting_tech_exception_test "Sumerian starting tech" game "Sumerian"
      (UnknownCivilization "Sumerian");
    starting_tech_exception_test "Viking starting tech" game "Viking"
      (UnknownCivilization "Viking");
    starting_tech_exception_test "Zulu starting tech" game "Zulu"
      (UnknownCivilization "Zulu");
    starting_tech_exception_test "American starting tech" game2 "American"
      (UnknownCivilization "American");
    starting_tech_exception_test "Chinese starting tech" game2 "Chinese"
      (UnknownCivilization "Chinese");
    starting_tech_exception_test "English starting tech" game2 "English"
      (UnknownCivilization "English");
    starting_tech_exception_test "French starting tech" game2 "French"
      (UnknownCivilization "French");
    starting_tech_exception_test "Russian starting tech" game2 "Russian"
      (UnknownCivilization "Russian");
    starting_tech_exception_test "Celtic starting tech" game2 "Celtic"
      (UnknownCivilization "Celtic");
    starting_tech_exception_test "Dutch starting tech" game2 "Dutch"
      (UnknownCivilization "Dutch");
    starting_tech_exception_test "Egyptian starting tech" game2 "Egyptian"
      (UnknownCivilization "Egyptian");
    starting_tech_exception_test "Ethiopian starting tech" game2 "Ethiopian"
      (UnknownCivilization "Ethiopian");
    starting_tech_exception_test "German starting tech" game2 "German"
      (UnknownCivilization "German");
    starting_tech_exception_test "Greek starting tech" game2 "Greek"
      (UnknownCivilization "Greek");
    starting_tech_exception_test "Holy Roman starting tech" game2 "Holy Roman"
      (UnknownCivilization "Holy Roman");
    starting_tech_exception_test "Incan starting tech" game2 "Incan"
      (UnknownCivilization "Incan");
    starting_tech_exception_test "Indian starting tech" game2 "Indian"
      (UnknownCivilization "Indian");
    starting_tech_exception_test "Japanese starting tech" game2 "Japanese"
      (UnknownCivilization "Japanese");
    starting_tech_exception_test "Khmer starting tech" game2 "Khmer"
      (UnknownCivilization "Khmer");
    starting_tech_exception_test "Korean starting tech" game2 "Korean"
      (UnknownCivilization "Korean");
    starting_tech_exception_test "Malinese starting tech" game2 "Malinese"
      (UnknownCivilization "Malinese");
    starting_tech_exception_test "Mayan starting tech" game2 "Mayan"
      (UnknownCivilization "Mayan");
    starting_tech_exception_test "Mongolian starting tech" game2 "Mongolian"
      (UnknownCivilization "Mongolian");
    starting_tech_exception_test "Native American starting tech" game2
      "Native American" (UnknownCivilization "Native American");
    starting_tech_exception_test "Ottoman starting tech" game2 "Ottoman"
      (UnknownCivilization "Ottoman");
    starting_tech_exception_test "Persian starting tech" game2 "Persian"
      (UnknownCivilization "Persian");
    starting_tech_exception_test "Portuguese starting tech" game2 "Portuguese"
      (UnknownCivilization "Portuguese");
    starting_tech_exception_test "Roman starting tech" game2 "Roman"
      (UnknownCivilization "Roman");
    starting_tech_exception_test "Spanish starting tech" game2 "Spanish"
      (UnknownCivilization "Spanish");
    starting_tech_exception_test "Sumerian starting tech" game2 "Sumerian"
      (UnknownCivilization "Sumerian");
    starting_tech_exception_test "Viking starting tech" game2 "Viking"
      (UnknownCivilization "Viking");
    starting_tech_exception_test "Zulu starting tech" game2 "Zulu"
      (UnknownCivilization "Zulu");
  ]

let unique_unit_test name adv civ expected_output =
  name >:: fun _ ->
  assert_equal expected_output (unique_unit adv civ) ~printer:pp_string

let unique_unit_tests =
  [
    unique_unit_test "American unique unit" game "American" "Navy SEAL";
    unique_unit_test "Chinese unique unit" game "Chinese" "Cho-Ko-Nu";
    unique_unit_test "English unique unit" game "English" "Redcoat";
    unique_unit_test "French unique unit" game "French" "Musketeer";
    unique_unit_test "Russian unique unit" game "Russian" "Cossack";
    unique_unit_test "Arabian unique unit" game2 "Arabian" "Camel Archer";
    unique_unit_test "Aztec unique unit" game2 "Aztec" "Jaguar";
    unique_unit_test "Babylonian unique unit" game2 "Babylonian" "Bowman";
    unique_unit_test "Byzantine unique unit" game2 "Byzantine" "Cataphract";
    unique_unit_test "Carthaginian unique unit" game2 "Carthaginian"
      "Numidian Cavalry";
  ]

let unique_unit_exception_test name adv room expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> unique_unit adv room)

let unique_unit_exception_tests =
  [
    unique_unit_exception_test "Arabian unique unit" game "Arabian"
      (UnknownCivilization "Arabian");
    unique_unit_exception_test "Aztec unique unit" game "Aztec"
      (UnknownCivilization "Aztec");
    unique_unit_exception_test "Babylonian unique unit" game "Babylonian"
      (UnknownCivilization "Babylonian");
    unique_unit_exception_test "Byzantine unique unit" game "Byzantine"
      (UnknownCivilization "Byzantine");
    unique_unit_exception_test "Carthaginian unique unit" game "Carthaginian"
      (UnknownCivilization "Carthaginian");
    unique_unit_exception_test "Celtic unique unit" game "Celtic"
      (UnknownCivilization "Celtic");
    unique_unit_exception_test "Dutch unique unit" game "Dutch"
      (UnknownCivilization "Dutch");
    unique_unit_exception_test "Egyptian unique unit" game "Egyptian"
      (UnknownCivilization "Egyptian");
    unique_unit_exception_test "Ethiopian unique unit" game "Ethiopian"
      (UnknownCivilization "Ethiopian");
    unique_unit_exception_test "German unique unit" game "German"
      (UnknownCivilization "German");
    unique_unit_exception_test "Greek unique unit" game "Greek"
      (UnknownCivilization "Greek");
    unique_unit_exception_test "Holy Roman unique unit" game "Holy Roman"
      (UnknownCivilization "Holy Roman");
    unique_unit_exception_test "Incan unique unit" game "Incan"
      (UnknownCivilization "Incan");
    unique_unit_exception_test "Indian unique unit" game "Indian"
      (UnknownCivilization "Indian");
    unique_unit_exception_test "Japanese unique unit" game "Japanese"
      (UnknownCivilization "Japanese");
    unique_unit_exception_test "Khmer unique unit" game "Khmer"
      (UnknownCivilization "Khmer");
    unique_unit_exception_test "Korean unique unit" game "Korean"
      (UnknownCivilization "Korean");
    unique_unit_exception_test "Malinese unique unit" game "Malinese"
      (UnknownCivilization "Malinese");
    unique_unit_exception_test "Mayan unique unit" game "Mayan"
      (UnknownCivilization "Mayan");
    unique_unit_exception_test "Mongolian unique unit" game "Mongolian"
      (UnknownCivilization "Mongolian");
    unique_unit_exception_test "Native American unique unit" game
      "Native American" (UnknownCivilization "Native American");
    unique_unit_exception_test "Ottoman unique unit" game "Ottoman"
      (UnknownCivilization "Ottoman");
    unique_unit_exception_test "Persian unique unit" game "Persian"
      (UnknownCivilization "Persian");
    unique_unit_exception_test "Portuguese unique unit" game "Portuguese"
      (UnknownCivilization "Portuguese");
    unique_unit_exception_test "Roman unique unit" game "Roman"
      (UnknownCivilization "Roman");
    unique_unit_exception_test "Spanish unique unit" game "Spanish"
      (UnknownCivilization "Spanish");
    unique_unit_exception_test "Sumerian unique unit" game "Sumerian"
      (UnknownCivilization "Sumerian");
    unique_unit_exception_test "Viking unique unit" game "Viking"
      (UnknownCivilization "Viking");
    unique_unit_exception_test "Zulu unique unit" game "Zulu"
      (UnknownCivilization "Zulu");
    unique_unit_exception_test "American unique unit" game2 "American"
      (UnknownCivilization "American");
    unique_unit_exception_test "Chinese unique unit" game2 "Chinese"
      (UnknownCivilization "Chinese");
    unique_unit_exception_test "English unique unit" game2 "English"
      (UnknownCivilization "English");
    unique_unit_exception_test "French unique unit" game2 "French"
      (UnknownCivilization "French");
    unique_unit_exception_test "Russian unique unit" game2 "Russian"
      (UnknownCivilization "Russian");
    unique_unit_exception_test "Celtic unique unit" game2 "Celtic"
      (UnknownCivilization "Celtic");
    unique_unit_exception_test "Dutch unique unit" game2 "Dutch"
      (UnknownCivilization "Dutch");
    unique_unit_exception_test "Egyptian unique unit" game2 "Egyptian"
      (UnknownCivilization "Egyptian");
    unique_unit_exception_test "Ethiopian unique unit" game2 "Ethiopian"
      (UnknownCivilization "Ethiopian");
    unique_unit_exception_test "German unique unit" game2 "German"
      (UnknownCivilization "German");
    unique_unit_exception_test "Greek unique unit" game2 "Greek"
      (UnknownCivilization "Greek");
    unique_unit_exception_test "Holy Roman unique unit" game2 "Holy Roman"
      (UnknownCivilization "Holy Roman");
    unique_unit_exception_test "Incan unique unit" game2 "Incan"
      (UnknownCivilization "Incan");
    unique_unit_exception_test "Indian unique unit" game2 "Indian"
      (UnknownCivilization "Indian");
    unique_unit_exception_test "Japanese unique unit" game2 "Japanese"
      (UnknownCivilization "Japanese");
    unique_unit_exception_test "Khmer unique unit" game2 "Khmer"
      (UnknownCivilization "Khmer");
    unique_unit_exception_test "Korean unique unit" game2 "Korean"
      (UnknownCivilization "Korean");
    unique_unit_exception_test "Malinese unique unit" game2 "Malinese"
      (UnknownCivilization "Malinese");
    unique_unit_exception_test "Mayan unique unit" game2 "Mayan"
      (UnknownCivilization "Mayan");
    unique_unit_exception_test "Mongolian unique unit" game2 "Mongolian"
      (UnknownCivilization "Mongolian");
    unique_unit_exception_test "Native American unique unit" game2
      "Native American" (UnknownCivilization "Native American");
    unique_unit_exception_test "Ottoman unique unit" game2 "Ottoman"
      (UnknownCivilization "Ottoman");
    unique_unit_exception_test "Persian unique unit" game2 "Persian"
      (UnknownCivilization "Persian");
    unique_unit_exception_test "Portuguese unique unit" game2 "Portuguese"
      (UnknownCivilization "Portuguese");
    unique_unit_exception_test "Roman unique unit" game2 "Roman"
      (UnknownCivilization "Roman");
    unique_unit_exception_test "Spanish unique unit" game2 "Spanish"
      (UnknownCivilization "Spanish");
    unique_unit_exception_test "Sumerian unique unit" game2 "Sumerian"
      (UnknownCivilization "Sumerian");
    unique_unit_exception_test "Viking unique unit" game2 "Viking"
      (UnknownCivilization "Viking");
    unique_unit_exception_test "Zulu unique unit" game2 "Zulu"
      (UnknownCivilization "Zulu");
  ]

let unique_building_test name adv civ expected_output =
  name >:: fun _ ->
  assert_equal expected_output (unique_building adv civ) ~printer:pp_string

let unique_building_tests =
  [
    unique_building_test "American unique building" game "American" "Mall";
    unique_building_test "Chinese unique building" game "Chinese" "Pavilion";
    unique_building_test "English unique building" game "English"
      "Stock Exchange";
    unique_building_test "French unique building" game "French" "Salon";
    unique_building_test "Russian unique building" game "Russian"
      "Research Institute";
    unique_building_test "Arabian unique building" game2 "Arabian" "Madrassa";
    unique_building_test "Aztec unique building" game2 "Aztec"
      "Sacrificial Altar";
    unique_building_test "Babylonian unique building" game2 "Babylonian"
      "Garden";
    unique_building_test "Byzantine unique building" game2 "Byzantine"
      "Hippodrome";
    unique_building_test "Carthaginian unique building" game2 "Carthaginian"
      "Cothon";
  ]

let unique_building_exception_test name adv room expected_output =
  name >:: fun _ ->
  assert_raises expected_output (fun () -> unique_building adv room)

let unique_building_exception_tests =
  [
    unique_building_exception_test "Arabian unique building" game "Arabian"
      (UnknownCivilization "Arabian");
    unique_building_exception_test "Aztec unique building" game "Aztec"
      (UnknownCivilization "Aztec");
    unique_building_exception_test "Babylonian unique building" game
      "Babylonian" (UnknownCivilization "Babylonian");
    unique_building_exception_test "Byzantine unique building" game "Byzantine"
      (UnknownCivilization "Byzantine");
    unique_building_exception_test "Carthaginian unique building" game
      "Carthaginian" (UnknownCivilization "Carthaginian");
    unique_building_exception_test "Celtic unique building" game "Celtic"
      (UnknownCivilization "Celtic");
    unique_building_exception_test "Dutch unique building" game "Dutch"
      (UnknownCivilization "Dutch");
    unique_building_exception_test "Egyptian unique building" game "Egyptian"
      (UnknownCivilization "Egyptian");
    unique_building_exception_test "Ethiopian unique building" game "Ethiopian"
      (UnknownCivilization "Ethiopian");
    unique_building_exception_test "German unique building" game "German"
      (UnknownCivilization "German");
    unique_building_exception_test "Greek unique building" game "Greek"
      (UnknownCivilization "Greek");
    unique_building_exception_test "Holy Roman unique building" game
      "Holy Roman" (UnknownCivilization "Holy Roman");
    unique_building_exception_test "Incan unique building" game "Incan"
      (UnknownCivilization "Incan");
    unique_building_exception_test "Indian unique building" game "Indian"
      (UnknownCivilization "Indian");
    unique_building_exception_test "Japanese unique building" game "Japanese"
      (UnknownCivilization "Japanese");
    unique_building_exception_test "Khmer unique building" game "Khmer"
      (UnknownCivilization "Khmer");
    unique_building_exception_test "Korean unique building" game "Korean"
      (UnknownCivilization "Korean");
    unique_building_exception_test "Malinese unique building" game "Malinese"
      (UnknownCivilization "Malinese");
    unique_building_exception_test "Mayan unique building" game "Mayan"
      (UnknownCivilization "Mayan");
    unique_building_exception_test "Mongolian unique buildingh" game "Mongolian"
      (UnknownCivilization "Mongolian");
    unique_building_exception_test "Native American unique building" game
      "Native American" (UnknownCivilization "Native American");
    unique_building_exception_test "Ottoman unique building" game "Ottoman"
      (UnknownCivilization "Ottoman");
    unique_building_exception_test "Persian unique building" game "Persian"
      (UnknownCivilization "Persian");
    unique_building_exception_test "Portuguese unique building" game
      "Portuguese" (UnknownCivilization "Portuguese");
    unique_building_exception_test "Roman unique building" game "Roman"
      (UnknownCivilization "Roman");
    unique_building_exception_test "Spanish unique building" game "Spanish"
      (UnknownCivilization "Spanish");
    unique_building_exception_test "Sumerian unique building" game "Sumerian"
      (UnknownCivilization "Sumerian");
    unique_building_exception_test "Viking unique building" game "Viking"
      (UnknownCivilization "Viking");
    unique_building_exception_test "Zulu unique building" game "Zulu"
      (UnknownCivilization "Zulu");
    unique_building_exception_test "American unique building" game2 "American"
      (UnknownCivilization "American");
    unique_building_exception_test "Chinese unique building" game2 "Chinese"
      (UnknownCivilization "Chinese");
    unique_building_exception_test "English unique building" game2 "English"
      (UnknownCivilization "English");
    unique_building_exception_test "French unique building" game2 "French"
      (UnknownCivilization "French");
    unique_building_exception_test "Russian unique building" game2 "Russian"
      (UnknownCivilization "Russian");
    unique_building_exception_test "Celtic unique building" game2 "Celtic"
      (UnknownCivilization "Celtic");
    unique_building_exception_test "Dutch unique building" game2 "Dutch"
      (UnknownCivilization "Dutch");
    unique_building_exception_test "Egyptian unique building" game2 "Egyptian"
      (UnknownCivilization "Egyptian");
    unique_building_exception_test "Ethiopian unique building" game2 "Ethiopian"
      (UnknownCivilization "Ethiopian");
    unique_building_exception_test "German unique building" game2 "German"
      (UnknownCivilization "German");
    unique_building_exception_test "Greek unique building" game2 "Greek"
      (UnknownCivilization "Greek");
    unique_building_exception_test "Holy Roman unique building" game2
      "Holy Roman" (UnknownCivilization "Holy Roman");
    unique_building_exception_test "Incan unique building" game2 "Incan"
      (UnknownCivilization "Incan");
    unique_building_exception_test "Indian unique building" game2 "Indian"
      (UnknownCivilization "Indian");
    unique_building_exception_test "Japanese unique building" game2 "Japanese"
      (UnknownCivilization "Japanese");
    unique_building_exception_test "Khmer unique building" game2 "Khmer"
      (UnknownCivilization "Khmer");
    unique_building_exception_test "Korean unique building" game2 "Korean"
      (UnknownCivilization "Korean");
    unique_building_exception_test "Malinese unique building" game2 "Malinese"
      (UnknownCivilization "Malinese");
    unique_building_exception_test "Mayan unique building" game2 "Mayan"
      (UnknownCivilization "Mayan");
    unique_building_exception_test "Mongolian unique building" game2 "Mongolian"
      (UnknownCivilization "Mongolian");
    unique_building_exception_test "Native American unique building" game2
      "Native American" (UnknownCivilization "Native American");
    unique_building_exception_test "Ottoman unique building" game2 "Ottoman"
      (UnknownCivilization "Ottoman");
    unique_building_exception_test "Persian unique building" game2 "Persian"
      (UnknownCivilization "Persian");
    unique_building_exception_test "Portuguese unique building" game2
      "Portuguese" (UnknownCivilization "Portuguese");
    unique_building_exception_test "Roman unique building" game2 "Roman"
      (UnknownCivilization "Roman");
    unique_building_exception_test "Spanish unique building" game2 "Spanish"
      (UnknownCivilization "Spanish");
    unique_building_exception_test "Sumerian unique building" game2 "Sumerian"
      (UnknownCivilization "Sumerian");
    unique_building_exception_test "Viking unique building" game2 "Viking"
      (UnknownCivilization "Viking");
    unique_building_exception_test "Zulu unique building" game2 "Zulu"
      (UnknownCivilization "Zulu");
  ]

let player_tests =
  List.flatten
    [
      civilization_ids_tests;
      leaders_tests;
      leaders_exception_tests;
      starting_tech_tests;
      starting_tech_exception_tests;
      unique_unit_tests;
      unique_unit_exception_tests;
      unique_building_tests;
      unique_building_exception_tests;
    ]

(*************************** State tests *******************************)

let civ4 = Yojson.Basic.from_file (data_dir_prefix ^ "civilization.json")
let civ4 = from_json civ4
(* all possible country/tech/leader combos *)
let player = initialize_state "English" "Churchill" "Fishing"
let player_english_churchill_mining = initialize_state "English" "Churchill" "Mining"
let player_english_victoria_fishing = initialize_state "English" "Victoria" "Fishing"
let player_english_victoria_mining = initialize_state "English" "Victoria" "Mining"
let player_english_elizabeth_mining = initialize_state "English" "Elizabeth" "Mining"
let player_english_elizabeth_fishing = initialize_state "English" "Elizabeth" "Fishing"
let player_china_maozedong_agriculture = initialize_state "China" "Mao Zedong" "Agriculture"
let player_china_maozedong_mining = initialize_state "China" "Mao Zedong" "Mining"
let player_china_qinshihuang_agriculture = initialize_state "China" "Qin Shi Huang" "Agriculture"
let player_china_qinshihuang_mining = initialize_state "China" "Qin Shi Huang" "Mining"
let player_russia_catherine_mining = initialize_state "Russia" "Catherine" "Mining"
let player_russia_catherine_hunting = initialize_state "Russia" "Catherine" "Hunting"
let player_russia_peter_mining = initialize_state "Russia" "Peter" "Mining"
let player_russia_peter_hunting = initialize_state "Russia" "Peter" "Hunting"
let player_russia_stalin_mining = initialize_state "Russia" "Stalin" "Mining"
let player_russia_stalin_hunting = initialize_state "Russia" "Stalin" "Hunting"
let player_america_lincoln_fishing = initialize_state "American" "Lincoln" "Fishing"
let player_america_lincoln_agriculture = initialize_state "American" "Lincoln" "Agriculture"
let player_america_roosevelt_fishing = initialize_state "American" "Roosevelt" "Fishing"
let player_america_roosevelt_agriculture = initialize_state "American" "Roosevelt" "Agriculture"
let player_america_wash_fishing = initialize_state "American" "Washington" "Fishing"
let player_america_wash_agriculture = initialize_state "American" "Washington" "Agriculture"
let player_france_napoleon_wheel = initialize_state "French" "Napoleon" "The Wheel"
let player_france_napoleon_agriculture = initialize_state "French" "Napoleon" "Agriculture"
let player_france_gaulle_wheel = initialize_state "French" "De Gaulle" "The Wheel"
let player_france_gaulle_agriculture = initialize_state "French" "De Gaulle" "Agriculture"
let player_france_louis_agriculture = initialize_state "French" "Louis XIV" "Agriculture"
let player_france_louis_wheel = initialize_state "French" "Louis XIV" "The Wheel"

let opponent = opp civ4 "English"

let state_tests =
  [
    ("initialize_state" >:: fun _ -> 
    assert_equal "Churchill" (get_leader player );
    assert_equal "English" (get_civilization_id player);
    assert_equal "Fishing" (get_starting_tech player);
    assert_equal 0 (get_score player);
    (* get_leader *)
    assert_equal "Mao Zedong" (get_leader player_china_maozedong_agriculture);
    assert_equal "Qin Shi Huang" (get_leader player_china_qinshihuang_agriculture);
    assert_equal "Lincoln" (get_leader player_america_lincoln_fishing);
    assert_equal "Washington" (get_leader player_america_wash_fishing);
    assert_equal "Roosevelt" (get_leader player_america_roosevelt_agriculture);
    assert_equal "Napoleon" (get_leader player_france_napoleon_wheel);
    assert_equal "Louis XIV" (get_leader player_france_louis_agriculture);
    assert_equal "De Gaulle" (get_leader player_france_gaulle_agriculture);
    assert_equal "Catherine" (get_leader player_russia_catherine_mining);
    assert_equal "Peter" (get_leader player_russia_peter_hunting);
    assert_equal "Stalin" (get_leader player_russia_stalin_mining);
    (* get_civilization_id *)
    assert_equal "Russia" (get_civilization_id player_russia_catherine_mining);
    assert_equal "Russia" (get_civilization_id player_russia_peter_hunting);
    assert_equal "Russia" (get_civilization_id player_russia_stalin_hunting);
    assert_equal "China" (get_civilization_id player_china_maozedong_agriculture);
    assert_equal "China" (get_civilization_id player_china_qinshihuang_agriculture);
    assert_equal "American" (get_civilization_id player_america_lincoln_fishing);
    assert_equal "French" (get_civilization_id player_france_napoleon_wheel);
    assert_equal "French" (get_civilization_id player_france_louis_agriculture);
    (* get_starting_tech *)
    assert_equal "Agriculture" (get_starting_tech player_china_maozedong_agriculture);
    assert_equal "Mining" (get_starting_tech player_china_maozedong_mining);
    assert_equal "Agriculture" (get_starting_tech player_china_qinshihuang_agriculture);
    assert_equal "Mining" (get_starting_tech player_china_qinshihuang_mining);
    assert_equal "Mining" (get_starting_tech player_russia_catherine_mining);
    assert_equal "Hunting" (get_starting_tech player_russia_catherine_hunting);
    assert_equal "Hunting" (get_starting_tech player_russia_peter_hunting);
    assert_equal "Mining" (get_starting_tech player_russia_peter_mining);
    assert_equal "Hunting" (get_starting_tech player_russia_stalin_hunting);
    assert_equal "Mining" (get_starting_tech player_russia_stalin_mining);
    assert_equal "Fishing" (get_starting_tech player_america_lincoln_fishing);
    assert_equal "Agriculture" (get_starting_tech player_america_lincoln_agriculture);
    assert_equal "Fishing" (get_starting_tech player_america_roosevelt_fishing);
    assert_equal "Agriculture" (get_starting_tech player_america_roosevelt_agriculture);
    assert_equal "Fishing" (get_starting_tech player_america_wash_fishing);
    assert_equal "Agriculture" (get_starting_tech player_america_wash_agriculture);
    assert_equal "The Wheel" (get_starting_tech player_france_napoleon_wheel);
    assert_equal "Agriculture" (get_starting_tech player_france_napoleon_agriculture);
    assert_equal "The Wheel" (get_starting_tech player_france_gaulle_wheel);
    assert_equal "Agriculture" (get_starting_tech player_france_gaulle_agriculture);
    assert_equal "The Wheel" (get_starting_tech player_france_louis_wheel);
    assert_equal "Agriculture" (get_starting_tech player_france_louis_agriculture);
    (* gamestart: should have empty claimed and built coordinates*)
    assert_equal [] (get_built_coordinates player_china_qinshihuang_mining);
    assert_equal [] (get_claimed_coordinates player_china_qinshihuang_mining);
    );
    ( "update_score and get_score" >:: fun _ ->
      assert (
        update_score player 0;
        update_score player 10;
        get_score player = 10;
        );
      assert(
        update_score player (-10);
        get_score player = 0;
        )
      );
    ( "update_claimed and get_claimed_coordinates" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        update_claimed player (Some x); 
        get_claimed_coordinates player = [ x ]);
    );
    ( "update_claimed with None" >:: fun _ ->
      assert (
        update_claimed opponent None;
        get_claimed_coordinates opponent = []) );
    ("update_built with None and empty list" >:: fun _ ->
      assert(
        update_built opponent None;
        get_built_coordinates opponent = [])
      );
    ( "update_built and get_built_coordinates" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        update_built player (Some x);
        get_built_coordinates player = [ x ]) );
    ("update_built on a nonempty list" >:: fun _ -> 
      assert (let x = generate_tile 1 1 in
      let x_1 = generate_tile 1 2 in  
      update_built player_america_lincoln_agriculture (Some x);
      update_built player_america_lincoln_agriculture (Some x_1);
      get_built_coordinates player_america_lincoln_agriculture = [x_1; x]);
    );
    ( "update_country_claimed_of_tile with None" >:: fun _ ->
      assert (
        update_country_claimed_of_tile None "American";
        (* Ensure no crash or change in state *) true) );
    ( "update_country_built_of_tile with None" >:: fun _ ->
      assert (
        update_country_built_of_tile None "American";
        (* Ensure no crash or change in state *) true) );
    ( "is_unclaimed, true with Some" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        update_country_claimed_of_tile (get_tile x 0 0) "English";
        not (is_unclaimed x 0 0)) );
    ( "can_claim, true" >:: fun _ ->
      assert (
        let y = generate_map 10 10 10 in
        let x = generate_tile 0 1 in
        let st = player in
        update_claimed st (Some x);
        can_claim y st 0 0) );
    ( "can_claim, false" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        let st = player in
        not (can_claim x st 12 0)) );
    ("can_claim on an already claimed tile, should be false" >:: fun _ -> 
      assert(
        let m = generate_map 10 10 10 in 
        let p1 = player_america_lincoln_agriculture in 
        let p2 = player_china_maozedong_agriculture in 
        let contested_tile = generate_tile 0 0 in 
        update_claimed p2 (Some contested_tile); 
        not(can_claim m p1 0 0)
      )
    );
    ( "update_claimed with a non-empty list" >:: fun _ ->
      let test_player = initialize_state "" "" "" in
      assert (
        let x = generate_tile 0 1 in
        let x_1 = generate_tile 0 2 in 
        update_claimed test_player (Some x);
        update_claimed test_player (Some x_1);
        get_claimed_coordinates test_player = [x_1; x]) );
    ("is_unclaimed, false case" >:: fun _ -> 
      let x = generate_map 10 10 10 in
      assert_bool "is_unclaimed is true" (is_unclaimed x 0 0)
      );

    ( "update_score multiple times" >:: fun _ ->
      assert (
        update_score player (-get_score player);
        update_score player 10;
        update_score player 5;
        update_score player 20;
        get_score player = 35) );
    ( "update_built with None" >:: fun _ ->
      assert (
        update_built opponent None;
        get_built_coordinates opponent = []) );
    ( "get_civilization_id" >:: fun _ ->
      assert (get_civilization_id player = "English") );
    ( "get_country_claimed" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        get_country_claimed x = "Unclaimed") );
    ( "get_country_built" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        get_country_built x = "Unbuilt") );
    ("get_leader" >:: fun _ -> assert (get_leader player = "Churchill"));
    ( "get_starting_tech" >:: fun _ ->
      assert (get_starting_tech player = "Fishing") );
    ("can_build with None" >:: fun _ -> assert (not (can_build player None)));
    ( "can_build without claim" >:: fun _ ->
      assert (
        let x = generate_tile 5 5 in
        not (can_build player (Some x))) );
    ( "update_country_claimed_of_tile with None" >:: fun _ ->
      assert (
        update_country_claimed_of_tile None "American";
        (* Ensure no crash or change in state *) true) );
    ( "update_country_built_of_tile with None" >:: fun _ ->
      assert (
        update_country_built_of_tile None "American";
        (* Ensure no crash or change in state *) true) );
    ( "get_tile out of bounds" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        get_tile x 15 15 = None) );
    ( "is_unclaimed, true with Some" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        update_country_claimed_of_tile (get_tile x 0 0) "English";
        not (is_unclaimed x 0 0)) );
    ( "update_country_claimed_of_tile" >:: fun _ ->
      assert (
        let x = generate_tile 5 5 in
        update_country_claimed_of_tile (Some x) "American";
        get_country_claimed x = "American") );
    ( "update_country_built_of_tile" >:: fun _ ->
      assert (
        let x = generate_tile 5 5 in
        update_country_built_of_tile (Some x) "American";
        get_country_built x = "American") );
    ( "get_x" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        get_x x = 0) );
    ( "get_y" >:: fun _ ->
      assert (
        let x = generate_tile 0 1 in
        get_y x = 1) );
    ( "generate_map and get_tile returns None" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        get_tile x 15 15 = None) );
    ("generate_map, valid get_tile call" >:: fun _ -> 
      assert(
        let x = generate_map 10 10 10 in
        not(get_tile x 2 2 = None) 
        )
      );
    ( "is_unclaimed, true" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        is_unclaimed x 0 0) );
    ( "is_unclaimed, false" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        update_country_claimed_of_tile (get_tile x 0 0) "English";
        not (is_unclaimed x 0 0)) );
    ( "can_claim, true" >:: fun _ ->
      assert (
        let y = generate_map 10 10 10 in
        let x = generate_tile 0 1 in
        update_claimed player (Some x);
        can_claim y player 0 0) );
    ( "can_claim, false" >:: fun _ ->
      assert (
        let x = generate_map 10 10 10 in
        not (can_claim x player 12 0)) );
    ( "determine_result" >:: fun _ ->
      let test_state_tie = initialize_state "" "" "" in 
      assert(
        let status = determine_result test_state_tie 0 in 
        status = "You tied!"
      );
      assert(
        let test_state_win = initialize_state "" "" "" in 
        update_score test_state_win 10;
        determine_result test_state_win 1 = "You won!"
      );
      assert(
        let test_state_lose = initialize_state "" "" "" in
        determine_result test_state_lose 100 = "You lost!"
      )
    );
    ("tile initilization" >:: fun _ -> 
      assert(
        let starting_tile = generate_tile 0 0 in 
        (get_country_built starting_tile = "Unbuilt")
      );
      assert(
        let starting_tile = generate_tile 0 0 in 
        (get_country_claimed starting_tile = "Unclaimed")
      );
    );
    ("tile icon tests" >:: fun _ -> 
      let ocean = "\u{1F30A}" in 
      let forest = "\u{1F332}" in 
      let grassland = "\u{1F33E}" in 
      let mountains = "\u{1F5FB}" in 
      let desert = "\u{1F42A}" in 
      assert(
        let random_tile = generate_tile 0 0 in 
        let terrain_str = tile_terrain(get_terrain random_tile) in 
        terrain_str = ocean
        || terrain_str = forest
        || terrain_str = grassland
        || terrain_str = mountains 
        || terrain_str= desert          
      );
    );
    ("tile bg color tests" >:: fun _ -> 
      assert(
        let english_tile = generate_tile 1 1 in 
        update_country_claimed_of_tile (Some english_tile) "English";
        update_claimed player_english_elizabeth_fishing (Some english_tile);
        tile_background_color english_tile = 22;
      );
      assert(
        let ameri_tile = generate_tile 1 1 in 
        update_country_claimed_of_tile (Some ameri_tile) "American";
        update_claimed player_english_elizabeth_fishing (Some ameri_tile);
        tile_background_color ameri_tile = 97 
      );
      assert(
        let rus_tile = generate_tile 1 1 in 
        update_country_claimed_of_tile (Some rus_tile) "Russian";
        update_claimed player_english_elizabeth_fishing (Some rus_tile);
        tile_background_color rus_tile = 58 
      );
      assert(
        let fren_tile = generate_tile 1 1 in 
        update_country_claimed_of_tile (Some fren_tile) "French";
        update_claimed player_english_elizabeth_fishing (Some fren_tile);
        tile_background_color fren_tile = 24
      );
      assert(
        let chinese_tile = generate_tile 1 1 in 
        update_country_claimed_of_tile (Some chinese_tile) "Chinese";
        update_claimed player_english_elizabeth_fishing (Some chinese_tile);
        tile_background_color chinese_tile = 202 
      );
      assert(
        let unclaimed_tile = generate_tile 1 1 in 
        tile_background_color unclaimed_tile = 255 
      );
    )
    

  ]
  let turn_tests : test list = [
    ("next_val" >:: fun _ -> 
      assert_equal 1 (next_val () );
      assert_equal 2 (next_val ());
      assert_equal 3 (next_val ());
    );
    ("check_for_quit" >:: fun _ -> 
      assert_equal () (check_for_quit "not quit");
      assert_equal () (check_for_quit "Q uit");
      assert_equal () (check_for_quit "Q-uit");
      assert_equal () (check_for_quit "definitely not quit. Civ4 is so fun!");
      );
  ]

(***********************************************************************)

let suite = "test suite" >::: List.flatten [ player_tests; state_tests; turn_tests ]
let _ = run_test_tt_main suite
