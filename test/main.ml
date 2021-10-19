open OUnit2
open Covid
open Network
open State

(******************************************************************************
  Start Helper Functions.
 ******************************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists. That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates. Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

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

(**[get_net_rep filepath] parses the json file at [filepath] into
   Network.t*)
let get_net_rep filepath : Network.t =
  Yojson.Basic.from_file filepath |> from_json

(******************************************************************************
  End Helper Functions.
  Start test constructors.
 ******************************************************************************)
let state_test
    (name : string)
    (state_in : State.t)
    (expected_output : State.t) : test =
  name >:: fun _ -> assert_equal expected_output (update_state state_in)
(* ~printer *)

(******************************************************************************
  End test constructors.
 ******************************************************************************)

let network_tests = []

let basic_before = get_net_rep "data/basic_network.json"

let basic_after = get_net_rep "data/basic_network_stepped.json"

let state_tests =
  [
    state_test
      "basic_network.json stepped forward is \
       basic_network_stepped.json, assuming infection prob is \
       identically 1"
      basic_before basic_after;
  ]

let suite =
  "test suit for Covid_Simulator"
  >::: List.flatten [ network_tests; state_tests ]

let _ = run_test_tt_main suite