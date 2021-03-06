open Yojson.Basic.Util
open Typedefs

type line = Position.t * Position.t

type person_id = int

exception UnknownPerson of person_id

exception UnknownEdge of person_id * person_id

type mortality_rate = float

type graph = {
  nodes : (Position.t * Infected.t) list;
  edges : line list;
}

type edge_info = {
  distance : float;
  time : InteractionTime.t;
}

type attr = {
  infected : Infected.t;
  sociability : Sociability.t;
  mask : Mask.t;
  position : Position.t;
  vaccine_doses : Vaccine.t;
}

type population = {
  location : Location.t;
  density : Density.t;
}

type virus = {
  name : string;
  incubation_time : IncubationTime.t;
  mortality_rate : mortality_rate;
}

type edge = person_id * edge_info
(**The type of an edge*)

exception InvalidJSON

type person = {
  attributes : attr;
  neighbors : edge list;
}
(** The type of a person*)

type t = {
  population : population;
  virus : virus;
  network : (person_id * person) list;
}

(**[edge_from_json j] parses an edge object from a JSON into an edge*)
let edge_from_json j =
  ( j |> member "person_id" |> to_int,
    {
      distance = j |> member "distance" |> to_float;
      time =
        j
        |> member "interaction time"
        |> to_string |> InteractionTime.from_string;
    } )

(**[person_from_json j] parses a person oject from a JSON into a person*)
let person_from_json j =
  {
    attributes =
      {
        infected =
          j |> member "infected" |> to_string |> Infected.from_string;
        sociability =
          j |> member "sociability" |> to_string
          |> Sociability.from_string;
        mask = j |> member "mask" |> to_string |> Mask.from_string;
        vaccine_doses =
          j |> member "vaccine doses" |> to_int |> string_of_int
          |> Vaccine.from_string;
        position =
          j |> member "position" |> to_list
          |> List.map (fun x -> x |> to_int);
      };
    neighbors =
      j |> member "edges" |> to_list |> List.map edge_from_json;
  }

(**[make_person j] parses a person object from a JSON into a
   [(person_id,person)] pair*)
let make_person j = (j |> member "id" |> to_int, person_from_json j)

(**[make_population j] parses a population parameters object from a JSON
   into a population*)
let make_population j =
  {
    location =
      j |> member "location" |> to_string |> Location.from_string;
    density = j |> member "density" |> to_string |> Density.from_string;
  }

(**[make_virus j] parses a virus information object from a JSON into a
   virus*)
let make_virus j =
  {
    name = j |> member "virus name" |> to_string;
    incubation_time =
      j
      |> member "incubation time"
      |> to_string |> IncubationTime.from_string;
    mortality_rate = j |> member "mortality rate" |> to_float;
  }

let from_json j =
  {
    population = make_population (j |> member "population parameters");
    virus = make_virus (j |> member "virus information");
    network = j |> member "people" |> to_list |> List.map make_person;
  }

let head (net : t) =
  match net.network with [] -> raise InvalidJSON | h :: t -> fst h

let people net =
  let rec people_help = function
    | [] -> []
    | h :: t -> fst h :: people_help t
  in
  people_help net.network

let add_person net id attributes neighbors =
  {
    population = net.population;
    virus = net.virus;
    network = (id, { attributes; neighbors }) :: net.network;
  }

let empty_network pop virus = { population = pop; virus; network = [] }

let get_person net id = List.assoc id net.network

let neighbors net id =
  try
    let p = List.assoc id net.network in
    p.neighbors |> List.map (fun x -> fst x)
  with Not_found -> raise (UnknownPerson id)

let get_attributes net id =
  try
    let p = List.assoc id net.network in
    p.attributes
  with Not_found -> raise (UnknownPerson id)

let get_position net id = (get_attributes net id).position

let edge_information net id1 id2 =
  if List.mem_assoc id1 net.network then
    try
      let p = List.assoc id1 net.network in
      p.neighbors |> List.assoc id2
    with Not_found ->
      if List.mem_assoc id2 net.network then
        raise (UnknownEdge (id1, id2))
      else raise (UnknownPerson id2)
  else raise (UnknownPerson id1)

let create_graph net =
  let idlist = people net in
  let pi netw id =
    ( get_position netw id,
      let attr = get_attributes netw id in
      attr.infected )
  in
  let pi' = pi net in
  let ed netw2 id1 id2 =
    (get_position netw2 id1, get_position netw2 id2)
  in
  let ed' = ed net in
  let neighbors' = neighbors net in
  let edges =
    List.map
      (fun id ->
        let ed'' = ed' id in
        neighbors' id |> List.map ed'')
      idlist
    |> List.flatten
  in
  let nlist = List.map pi' idlist in
  { nodes = nlist; edges }

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

let edge_pp (e : edge) =
  Printf.sprintf "id: %i, edge_info: %s" (fst e)
    (Printf.sprintf "(distance: %F, time: %s)" (snd e).distance
       (InteractionTime.pp (snd e).time))

let neighbor_pp = pp_list edge_pp "; "

let attr_printer (a : attr) =
  Printf.sprintf
    "Status: %s; mask: %s; sociability: %s; vaccine doses: %s; \
     position: %s;"
    (Infected.pp a.infected)
    (Mask.pp a.mask)
    (Sociability.pp a.sociability)
    (Vaccine.pp a.vaccine_doses)
    (Position.pp a.position)

let person_printer (p : person) =
  Printf.sprintf "%s %s"
    (attr_printer p.attributes)
    (neighbor_pp p.neighbors)

let graph_tuple_printer (id, pers) =
  Printf.sprintf "id: %i; %s" id (person_printer pers)

let graph_printer (net : t) =
  pp_list graph_tuple_printer ";\n" net.network ^ "\n"

let pop_parameters net = net.population

let virus_info net = net.virus

let size net = List.length net.network

let fraction_infected net =
  let rec fi_helper = function
    | [] -> 0
    | (_, r) :: t -> begin
        match r.attributes.infected with
        | Infected -> 1 + fi_helper t
        | Not_infected -> fi_helper t
      end
  in
  float (fi_helper net.network) /. float (size net)

let transmission_probability net id1 id2 = 1.

let transmission_probability_real net id1 id2 =
  (*A.F: Transmission_probability uses various internal helper functions
    that evaluate the effect of all relevant parametets in the network
    and of the two people interacting in order to output a probability
    of person id1 being infected by person id2. Note: if id1 is already
    infected then the probability of infection is 0. R.I: The output is
    always between 0 and 1. *)
  let r = fraction_infected net in
  let dense = function
    (*Denser population -> higher transmission probability*)
    | High_density -> 0.9
    | Med_density -> 0.7
    | Low_density -> 0.5
  in
  let loc (*Indoor location -> higher transmission probability*) =
    function
    | Indoors -> 1.
    | Outdoors -> 0.7
  in
  let dist = function
    (*Closer interaction -> higher transmission probability*)
    | i when i >= 6. -> 0.2
    | i when 3. < i && i < 6. -> 0.6
    | i when 1. <= i && i <= 3. -> 0.95
    | i when 0. <= i && i < 1. -> 1.
    | _ -> raise InvalidJSON
  in
  let tim (*Longer interaction -> higher transmission probability*) =
    function
    | Long -> 1.
    | Regular -> 0.8
    | Short -> 0.4
  in
  let msk p1 p2 =
    (* Not wearing masks -> higher transmission probability. Note: if
       only the infected person is wearing a mask, the probability of
       transmitting virus is less than if only the non-infected person
       is wearing a mask*)
    match p2.mask with
    | Masked -> begin
        match p1.mask with Masked -> 0.6 | Not_masked -> 0.7
      end
    | Not_masked -> begin
        match p1.mask with Masked -> 0.9 | Not_masked -> 1.
      end
  in
  let doses p =
    (*More vaccine doses -> lower probability of infection*)
    match p.vaccine_doses with
    | Two_or_more -> 0.2
    | One -> 0.8
    | Zero -> 1.
  in
  let e = edge_information net id1 id2 in
  let attr1 = get_attributes net id1 in
  let attr2 = get_attributes net id2 in
  (*Use all functions defined above to calculate transmission
    probability*)
  dense net.population.density
  *. loc net.population.location
  *. r *. dist e.distance *. tim e.time *. msk attr1 attr2
  *. doses attr1

module PersonKey = struct
  type t = person_id

  let compare = Stdlib.compare
end

module NodeSet = Set.Make (PersonKey)

(* [visitedTracker] implements a logger for tracking which nodes have
   been visited during a graph traversal*)
class visitedTracker =
  object (self)
    val mutable visited = NodeSet.empty (* can i make this private?*)

    (* [add_visit n] updates tracker with the fact that node [n] has
       been visited *)
    method add_visit n = visited <- NodeSet.add n visited

    (* [was_visited n] is if node [n] has been visited before*)
    method was_visited (n : person_id) : bool = NodeSet.mem n visited

    (* [clear] deletes all entries from the tracker *)
    method clear = visited <- NodeSet.empty
  end

(* [rand_bool prob] generates [true] with probability [prob], otherwise
   [false] *)
let rand_bool prob : bool = Random.float 1. < prob

(* [process_node state n] is if node [n] in state [state] gets infected
   by its neighbors *)
let process_node state (n : person_id) =
  let rec lst_itr acc = function
    | h :: t -> lst_itr (transmission_probability state n h :: acc) t
    | [] -> acc
  in
  lst_itr []
    (neighbors state n
    |> List.filter (fun neighbor ->
           (get_attributes state neighbor).infected = Infected))
  |> List.map rand_bool
  |> List.exists (fun x -> x)

(* [generate_updates state tracker] is a map of people in state who get
   infected*)
let generate_updates state tracker =
  let rec generate_rec (node : person_id) inf_acc =
    tracker#add_visit node;
    let new_inf_acc =
      if process_node state node then NodeSet.add node inf_acc
      else inf_acc
    in
    let rec lst_itr = function
      | [] -> new_inf_acc
      | h :: t ->
          if tracker#was_visited h then lst_itr t
          else NodeSet.union (generate_rec h new_inf_acc) (lst_itr t)
    in
    lst_itr (neighbors state node)
  in
  generate_rec (head state) NodeSet.empty

(* [apply_changes state changes] parses through the graph and applies
   [changes]*)
let rec apply_changes (state : t) changes =
  let rec iterate_people net_acc = function
    | [] -> net_acc
    | h :: t ->
        let pers = get_person state h in
        let new_person =
          if NodeSet.mem h changes then
            {
              pers with
              attributes = { pers.attributes with infected = Infected };
            }
          else pers
        in
        iterate_people
          (add_person net_acc h new_person.attributes
             new_person.neighbors)
          t
  in
  iterate_people
    (empty_network (pop_parameters state) (virus_info state))
    (people state)

let update_state state =
  generate_updates state (new visitedTracker) |> apply_changes state
