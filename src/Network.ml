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

let edge_from_json j =
  ( j |> member "person_id" |> to_int,
    {
      distance = j |> member "distance" |> to_float;
      risk = j |> member "risk" |> to_string;
    } )

let person_from_json j =
  {
    attributes =
      {
        infected =
          j |> member "infected" |> to_string |> string_to_infected;
        mask = j |> member "mask" |> to_string;
        immunity = j |> member "immunity" |> to_float;
        position =
          j |> member "position" |> to_list
          |> List.map (fun x -> x |> to_int);
      };
    neighbors =
      j |> member "edges" |> to_list |> List.map edge_from_json;
  }

let make_person j = (j |> member "id" |> to_int, person_from_json j)

let from_json j =
  j |> member "people" |> to_list |> List.map make_person

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

let edge_printer (e : edge) =
  Printf.sprintf "id: %i, edge_info: %s" (fst e)
    (Printf.sprintf "(distance: %F, risk: %s)" (snd e).distance
       (snd e).risk)

let neighbor_printer = pp_list edge_printer "; "

let infected_printer = function
  | Infected -> "infected"
  | Not_infected -> "not infected"

let position_printer (pos : position) =
  Printf.sprintf "(%i, %i)" (List.hd pos) (List.hd (List.tl pos))

let attr_printer (a : attr) =
  Printf.sprintf "Status: %s; mask: %s; immunity: %F; position: %s;"
    (infected_printer a.infected)
    a.mask a.immunity
    (position_printer a.position)

let person_printer (p : person) =
  Printf.sprintf "%s %s"
    (attr_printer p.attributes)
    (neighbor_printer p.neighbors)

let graph_tuple_printer (id, pers) =
  Printf.sprintf "id: %i; %s" id (person_printer pers)

let graph_printer (net : t) =
  pp_list graph_tuple_printer ";\n" net ^ "\n"
