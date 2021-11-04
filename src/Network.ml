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
