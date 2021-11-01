open Yojson.Basic.Util

type person_id = int

type position = int list

type line = position * position

exception UnknownPerson of person_id

exception UnknownEdge of person_id * person_id

exception InvalidPosition of position

exception InvalidJSON

type infected =
  | Infected
  | Not_infected

type sociability =
  | Low
  | Medium
  | High

type mask =
  | Masked
  | Not_masked

type vaccine_doses =
  | Two_or_more
  | One
  | Zero

type interaction_time =
  | Short
  | Regular
  | Long

type location =
  | Indoors
  | Outdoors

type density =
  | Low_density
  | Med_density
  | High_density

type incubation_time =
  | Days of int
  | Weeks of int

type mortality_rate = float

type graph = {
  nodes : (position * infected) list;
  edges : line list;
}

type edge_info = {
  distance : float;
  time : interaction_time;
}

type attr = {
  infected : infected;
  sociability : sociability;
  mask : mask;
  position : position;
  vaccine_doses : vaccine_doses;
}

type population = {
  location : location;
  density : density;
}

type virus = {
  name : string;
  incubation_time : incubation_time;
  mortality_rate : mortality_rate;
}

type edge = person_id * edge_info
(**The type of an edge*)

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

(********* Helper json parsing functions **********)

(**string_to_infected [s] is conversion of [s] to its corresponding
   infected type. Raises Failure "Invalid JSON input" if [s] does not
   correspond to an infected type.*)
let string_to_infected = function
  | s when s = "yes" -> Infected
  | s when s = "no" -> Not_infected
  | s -> raise InvalidJSON

(**string_to_mask [s] is conversion of [s] to its corresponding mask
   type. Raises Failure "Invalid JSON input" if [s] does not correspond
   to a mask type.*)
let string_to_mask = function
  | s when s = "yes" -> Masked
  | s when s = "no" -> Not_masked
  | s -> raise InvalidJSON

(**int_to_vaccine_doses [i] is conversion of [i] to its corresponding
   vaccine_doses type. Raises Failure "Invalid JSON input" if [i] does
   not correspond to a vaccine_doses type.*)
let int_to_vaccine_doses = function
  | i when i >= 2 -> Two_or_more
  | i when i = 1 -> One
  | i when i = 0 -> Zero
  | i -> raise InvalidJSON

(**string_to_interaction_time [s] is conversion of [s] to its
   corresponding interaction_time type. Raises Failure "Invalid JSON
   input" if [s] does not correspond to a interaction_time type.*)
let string_to_interaciton_time = function
  | s when s = "long" -> Long
  | s when s = "short" -> Short
  | s when s = "regular" -> Regular
  | s -> raise InvalidJSON

(**string_to_density [s] is conversion of [s] to its corresponding
   density type. Raises Failure "Invalid JSON input" if [s] does not
   correspond to a density type.*)
let string_to_density = function
  | s when s = "high" -> High_density
  | s when s = "low" -> Low_density
  | s when s = "medium" -> Med_density
  | s -> raise InvalidJSON

(**string_to_incubation_time [s] is conversion of [s] to its
   corresponding incubation_time type. Raises Failure "Invalid JSON
   input" if [s] does not correspond to a incubation_time type.*)
let string_to_incubation_time = function
  | s when String.sub s 2 (String.length s) = "days" ->
      Days (int_of_char s.[0])
  | s when String.sub s 2 (String.length s) = "weeks" ->
      Weeks (int_of_char s.[0])
  | s -> raise InvalidJSON

(**string_to_location [s] is conversion of [s] to its corresponding
   location type. Raises Failure "Invalid JSON input" if [s] does not
   correspond to a location type.*)
let string_to_location = function
  | s when s = "indoors" -> Indoors
  | s when s = "outdoors" -> Outdoors
  | s -> raise InvalidJSON

(**string_to_sociability [s] is conversion of [s] to its corresponding
   sociability type. Raises Failure "Invalid JSON input" if [s] does not
   correspond to a sociability type.*)
let string_to_sociability = function
  | s when s = "low" -> Low
  | s when s = "high" -> High
  | s when s = "medium" -> Medium
  | s -> raise InvalidJSON

(********* End of helper json parsing functions **********)

(**[edge_from_json j] parses an edge object from a JSON into an edge*)
let edge_from_json j =
  ( j |> member "person_id" |> to_int,
    {
      distance = j |> member "distance" |> to_float;
      time =
        j
        |> member "interaction time"
        |> to_string |> string_to_interaciton_time;
    } )

(**[person_from_json j] parses a person oject from a JSON into a person*)
let person_from_json j =
  {
    attributes =
      {
        infected =
          j |> member "infected" |> to_string |> string_to_infected;
        sociability =
          j |> member "sociability" |> to_string
          |> string_to_sociability;
        mask = j |> member "mask" |> to_string |> string_to_mask;
        vaccine_doses =
          j |> member "vaccine doses" |> to_int |> int_to_vaccine_doses;
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
    location = j |> member "location" |> to_string |> string_to_location;
    density = j |> member "density" |> to_string |> string_to_density;
  }

(**[make_virus j] parses a virus information object from a JSON into a
   virus*)
let make_virus j =
  {
    name = j |> member "virus name" |> to_string;
    incubation_time =
      j
      |> member "incubation time"
      |> to_string |> string_to_incubation_time;
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
  (id, { attributes; neighbors }) :: net.network

let get_person net id = List.assoc id net.network

(******End possibly bad*******)

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

let pop_parameters net = net.population

let virus_info net = net.virus
