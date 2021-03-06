open OUnit2
open Covid
open Network

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

(**[get_net_rep filepath] parses the json file at [filepath] into
   Network.t*)
let get_net_rep filepath : Network.t =
  Yojson.Basic.from_file filepath |> from_json

(** [pp_list pp_elt demarc lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst], and using demarcation
    [demarc] to separate elements. *)
let pp_list pp_elt demarc lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ demarc) t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(******************************************************************************
  End Helper Functions.
  Start test constructors.
 ******************************************************************************)
let state_test
    (name : string)
    (state_in : Network.t)
    (expected_output : Network.t) : test =
  name >:: fun _ ->
  assert_equal expected_output (update_state state_in)
    ~printer:Network.graph_printer

(** [head_test name net expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [head net]. *)
let head_test name net expected_output : test =
  name >:: fun _ -> assert_equal expected_output (head net)

(** [neighbors_test name net id expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [neighbors net id]. *)
let neighbors_test name net id expected_output : test =
  name >:: fun _ -> assert_equal expected_output (neighbors net id)

(** [get_position_test name net id expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [get_position net id]. *)
let get_position_test name net id expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_position net id)

(** [get_attributes_test name net id expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [get_attributes net id]. *)
let get_attributes_test name net id expected_output : test =
  name >:: fun _ -> assert_equal expected_output (get_attributes net id)

(** [edge_information_test name net id1 id2 expected_output] constructs
    an OUnit test named [name] that asserts the quality of
    [expected_output] with [edge_information net id1 id2]. *)
let edge_information_test name net id1 id2 expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output (edge_information net id1 id2)

(** [virus_info_test name net expected_output] constructs an OUnit test
    named [name] that asserts the quality of [expected_output] with
    [virus_info net]. *)
let virus_info_test name net expected_output : test =
  name >:: fun _ -> assert_equal expected_output (virus_info net)

(** [pop_parameters_test name net expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output] with
    [pop_parameters net]. *)
let pop_parameters_test name net expected_output : test =
  name >:: fun _ -> assert_equal expected_output (pop_parameters net)

(** [size_test name net expected_output] constructs an OUnit test named
    [name] that asserts the quality of [expected_output] with
    [size net]. *)
let size_test name net expected_outout : test =
  name >:: fun _ -> assert_equal expected_outout (size net)

(** [fraction_infected_test name net expected_output] constructs an
    OUnit test named [name] that asserts the quality of
    [expected_output] with [fraction_infected net]. *)
let fraction_infected_test name net expected_outout : test =
  name >:: fun _ -> assert_equal expected_outout (fraction_infected net)

(******************************************************************************
  End test constructors. Start Initialize Testing Variables
  ******************************************************************************)

let net1 = get_net_rep "data/5_person_network.json"

let attr1 =
  {
    infected = Infected;
    mask = Masked;
    sociability = High;
    vaccine_doses = Zero;
    position = [ 1; 2 ];
  }

let attr2 =
  {
    infected = Infected;
    sociability = Medium;
    mask = Not_masked;
    vaccine_doses = Two_or_more;
    position = [ 2; 4 ];
  }

let attr5 =
  {
    infected = Not_infected;
    sociability = Medium;
    vaccine_doses = Two_or_more;
    mask = Masked;
    position = [ 1; 6 ];
  }

let edge_info12 = { distance = 3.5; time = Short }

let edge_info13 = { distance = 2.0; time = Regular }

let edge_info25 = { distance = 0.5; time = Long }

let pop = { location = Indoors; density = High_density }

let virus =
  {
    name = "SARS-CoV-2";
    incubation_time = Days 5;
    mortality_rate = 0.02;
  }

(******************************************************************************
  End Initialize Testing Variables.
  Start tests.  
 ******************************************************************************)
let network_tests =
  [
    head_test {| head of net1 is 1|} net1 1;
    neighbors_test {|neighbors of person 1 in net1 is [2;3]|} net1 1
      [ 2; 3 ];
    neighbors_test {|neighbors of person 2 in net1 is [1;4;5]|} net1 2
      [ 1; 4; 5 ];
    neighbors_test {|neighbors of person 3 in net1 is [1;4]|} net1 3
      [ 1; 4 ];
    get_position_test "position of person 1 in net1 is [1;2]" net1 1
      [ 1; 2 ];
    get_position_test "position of person 2 in net1 is [2;4]" net1 2
      [ 2; 4 ];
    get_attributes_test "attributes of person 1 in net1 is [attr1]" net1
      1 attr1;
    get_attributes_test "attributes of person 1 in net1 is [attr2]" net1
      2 attr2;
    edge_information_test
      "edge information of edge between person 1 and person 2 in net1 \
       is edge_info12"
      net1 1 2 edge_info12;
    edge_information_test
      "edge information of edge between person 2 and person 1 in net1 \
       is edge_info12"
      net1 2 1 edge_info12;
    edge_information_test
      "edge information of edge between person 1 and person 3 in net1 \
       is edge_info12"
      net1 1 3 edge_info13;
    edge_information_test
      "edge information of edge between person 3 and person 1 in net1 \
       is edge_info12"
      net1 3 1 edge_info13;
    pop_parameters_test
      "population_parameters of net1 should be {location = Indoors; \
       density = High_density}"
      net1 pop;
    virus_info_test
      "virus_info of net1 should be {name = SARS-CoV-2;ncubation_time \
       = Days 5;\n\
      \       mortality_rate = 0.02}" net1 virus;
    size_test "size of net1 should be 5" net1 5;
    fraction_infected_test
      "fraction of infected people in net 1 should be 0.6" net1 0.6;
    (************ Error testing ************)
    ( "neighbors of person 4 in net1 should raise UnknownPerson \
       exception"
    >:: fun _ ->
      assert_raises (UnknownPerson 6) (fun () -> neighbors net1 6) );
    ( "get_position of person 6 in net1 should raise UnknownPerson \
       exception"
    >:: fun _ ->
      assert_raises (UnknownPerson 6) (fun () -> get_position net1 6) );
    ( "get_attributes of person 6 in net1 should raise UnknownPerson \
       exception"
    >:: fun _ ->
      assert_raises (UnknownPerson 6) (fun () -> get_attributes net1 6)
    );
    ( "edge_information of person 1 and person 6 in net1 should raise \
       UnknownPerson exception"
    >:: fun _ ->
      assert_raises (UnknownPerson 6) (fun () ->
          edge_information net1 1 6) );
    ( "edge_information of person 6 and person 7 in net1 should raise \
       UnknownPerson exception"
    >:: fun _ ->
      assert_raises (UnknownPerson 6) (fun () ->
          edge_information net1 6 7) );
    ( "edge_information of person 2 and person 3 in net1 should raise \
       UnknownEdge exception"
    >:: fun _ ->
      assert_raises
        (UnknownEdge (2, 3))
        (fun () -> edge_information net1 2 3) );
  ]

(* let basic_before = get_net_rep "data/basic_network.json"

   let basic_after = get_net_rep "data/basic_network_stepped.json" *)
let five_before = get_net_rep "data/5_person_network.json"

let five_after = get_net_rep "data/5_person_network_stepped.json"

let state_tests =
  [
    state_test
      "5_person_network.json stepped forward\n\
      \   is  5_person_network_sptted.json, assuming infection prob is \
       identically 1 "
      five_before five_after;
    (* state_test "basic_network.json stepped forward is\n\ \
       basic_network_stepped.json, assuming infection prob is \
       identically\n\ \ 1" basic_before basic_after; *)
  ]

let suite =
  "test suit for Covid_Simulator"
  >::: List.flatten [ network_tests; state_tests ]

let _ = run_test_tt_main suite