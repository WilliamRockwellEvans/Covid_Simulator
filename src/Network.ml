open Yojson.Basic.Util

type person_id = int

type position = int list

type line = position * position

exception UnknownPerson of person_id

exception UnknownEdge of person_id * person_id

exception InvalidPosition of position

exception InvalidJSON

(**The type of the state of infection of a person*)
type infected =
  | Infected
  | Not_infected

type graph = {
  nodes : (position * infected) list;
  edges : line list;
}

type edge_info = {
  distance : float;
  risk : string;
}
(** The type of edge information.*)

type attr = {
  infected : infected;
  mask : string;
  immunity : float;
  position : position;
}
(** The type of individual attributes of person [id]*)

type edge = person_id * edge_info
(**The type of an edge*)

type person = {
  attributes : attr;
  neighbors : edge list;
}
(** The type of a person*)

type t = (person_id * person) list

(**string_of_infected [s] is conversion of [s] to its corresponding
   infected type. Raises Failure "Invalid JSON input" if [s] does not
   correspond to an infected type.*)
let string_to_infected = function
  | s when s = "Yes" -> Infected
  | s when s = "No" -> Not_infected
  | s -> raise InvalidJSON

let apply_unpackage f name h = member name h |> f

let unpackage_int = apply_unpackage to_int

let unpackage_flt = apply_unpackage to_float

let unpackage_str = apply_unpackage to_string

let unpackage_lst = apply_unpackage to_list

let edge_from_json j =
  ( j |> unpackage_int "person_id",
    {
      distance = j |> unpackage_flt "distance";
      risk = j |> unpackage_str "risk";
    } )

let person_from_json j =
  {
    attributes =
      {
        infected = j |> unpackage_str "infected" |> string_to_infected;
        mask = j |> unpackage_str "mask";
        immunity = j |> unpackage_flt "immunity";
        position =
          j
          |> unpackage_lst "position"
          |> List.map (fun x -> x |> to_int);
      };
    neighbors = j |> unpackage_lst "edges" |> List.map edge_from_json;
  }

let make_person j = (j |> unpackage_int "id", person_from_json j)

let from_json j = j |> unpackage_lst "people" |> List.map make_person

let head (net : t) =
  match net with [] -> raise InvalidJSON | h :: t -> fst h

let rec people = function [] -> [] | h :: t -> fst h :: people t

(*********Possibly bad below***********)
let empty_network : t = []

let add_person net id attributes neighbors =
  (id, { attributes; neighbors }) :: net

let get_person net id = List.assoc id net

(******End possibly bad*******)

let neighbors net id =
  try
    let p = List.assoc id net in
    p.neighbors |> List.map (fun x -> fst x)
  with Not_found -> raise (UnknownPerson id)

let get_attributes net id =
  try
    let p = List.assoc id net in
    p.attributes
  with Not_found -> raise (UnknownPerson id)

let get_position net id = (get_attributes net id).position

let edge_information net id1 id2 =
  if List.mem_assoc id1 net then
    try
      let p = List.assoc id1 net in
      p.neighbors |> List.assoc id2
    with Not_found ->
      if List.mem_assoc id2 net then raise (UnknownEdge (id1, id2))
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
