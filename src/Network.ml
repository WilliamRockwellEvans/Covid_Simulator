open Yojson.Basic.Util 

type person_id = int

type position = int list

exception UnknownPerson of person_id

exception UnknownEdge of person_id * person_id

exception InvalidPosition of position

exception InvalidJSON

(**The type of the state of infection of a person*)
type infected = 
| Infected
| Not_infected

(** The type of edge information.*)
type edge_info = {distance : float;
                    risk : string} 

(** The type of individual attributes of person [id]*)
type attr = {infected : infected ;
                    mask : string ;
                    immunity : float;
                    position : position}

(**The type of an edge*)
type edge = person_id * edge_info

(** The type of a person*)
type person = {attributes : attr; neighbors : edge list}

type t = (person_id * person) list

(**string_of_infected [s] is conversion of [s] to its corresponding 
infected type. Raises Failure "Invalid JSON input" if
  [s] does not correspond to an infected type.*)
let string_to_infected = function 
| s when s = "Yes" -> Infected
| s when s = "No" -> Not_infected
| s -> raise InvalidJSON


let edge_from_json j = j |> member "person_id" |> to_int,{
  distance = j |> member "distance" |> to_float;
  risk = j |> member "risk" |> to_string
}

let person_from_json j = {attributes = {
  infected = j |> member "infected" |> to_string |> string_to_infected;
  mask = j |> member "mask" |> to_string;
  immunity= j |> member "immunity" |> to_float;
  position = j |> member "position" |> to_list |> List.map (fun x -> x |> to_int)
}; neighbors = j |> member "edges" |> to_list |> List.map edge_from_json}

let make_person j = j |> member "id" |> to_int, person_from_json j

let from_json j = 
 j |> member "people" |> to_list |> List.map make_person

let head = function 
| [] -> raise InvalidJSON
| h :: t -> snd h 

let neighbors net id = let p = List.assoc id net in 
p.neighbors |> List.map (fun x -> fst x)

let get_position net id = try let p = List.assoc id net in p.position
with | Not_found -> raise (UnknownPerson id)

let get_attributes net id = try let p =  List.assoc id net in p.attributes 
with | Not_found -> raise (UnknownPerson id)

let edge_info net id1 id2 = 
  if (List.mem_assoc id1 net) then
    try let p = List.assoc id1 net 
   in p.neighbors |> List.assoc id2 
    with | Not_found -> raise (UnknownEdge (id1,id2))
  else raise (UnknownPerson id1)

