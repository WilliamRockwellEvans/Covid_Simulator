open Typedefs
(**Representation of static social network.

   This module handles the data stored in a JSON file with the
   information of network and virus of study. The JSON being handled
   includes the data of the people and their interactions as initial
   conditions, as well as the parameters of the network and the virus.
   This module creates a representation of the network and can be used
   to query information about the network to be used in a GUI*)

type t
(** The abstract type of the values representing infection network*)

type person_id = int
(** The type of person identifiers*)

type line = Position.t * Position.t
(** the type of a line between two points in a grid*)

type graph = {
  nodes : (Position.t * Status.t) list;
  edges : line list;
}
(** The type of a graph for the network*)

type edge_info = {
  distance : float;
  time : interaction_time;
}
(** The type of edge information.*)

type attr = {
  status : Status.t;
  sociability : Sociability.t;
  mask : mask;
  position : Position.t;
  vaccine_doses : Vaccine.t;
}
(** The type of individual attributes of person [id]*)

type population = {
  location : location;
  density : density;
}
(** The type of the population of the network*)

type virus = {
  name : string;
  incubation_time : incubation_time;
  mortality_rate : mortality_rate;
}
(**The type of the virus present in the network*)

exception UnknownPerson of person_id
(** Raised when [person_id] is not in the network*)

exception UnknownEdge of person_id * person_id
(** Raised when a non-existing edge is encountered*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the network that [j] represents. Requires: [j] is a
    valid JSON network representation. *)

val head : t -> person_id
(**[head net] is the first person in the network*)

val people : t -> person_id list
(**[people net] is the list of person ids in the network*)

(*********************This is likely bad************************)
type edge = person_id * edge_info

type person = {
  attributes : attr;
  neighbors : edge list;
}
(** The type of people*)

val add_person : t -> person_id -> attr -> edge list -> t
(** [add_person net id attributes neighbors] is the network formed by
    adding the person [id] with [attributes] and [neighbors] to [net].
    Requires: neighbors is a valid list of person_ids, [net] is a valid
    network, [person_id] is unique*)

val get_person : t -> person_id -> person

(*********************End bad*******************)

val neighbors : t -> person_id -> person_id list
(**[neighbors net p] is a set-like list of all the people who [p] has an
   interaction with in network [net]. Raises [UnknownPerson p] if the
   [p] is not a person in [net]*)

val get_position : t -> person_id -> Position.t
(** [get_position net p] is the Position.t of person [p] in network
    [net]. Raises [UnknownPerson p] if person [p] does not exist in
    [net]*)

val get_attributes : t -> person_id -> attr
(** [get_attributes net p] is the attributes of person [p] in network
    [net]. Raises [UnknownPerson p] if person [p] does not exist in
    [net]*)

val edge_information : t -> person_id -> person_id -> edge_info
(** [get_edge_info net p1 p2] is the edge information of the edge
    between person [p1] and person [p2]. Raised [UnknownPerson p1] if p1
    does not exist in [net]. Raises [UnknownEdge (p1, p2)] if an edge
    between [p1] and [p2] does not exist.*)

val create_graph : t -> graph
(** [create_graph net] is the graph type representation of the network
    net to bet used in the GUI *)

val graph_printer : t -> string
(** [graph_printer net] pretty prints [net]*)

val pop_parameters : t -> population
(** [pop_parameters net] is the population type with the parameters of
    network [net]*)

val virus_info : t -> virus
(** [virus_info net] is the virus type with the information of the virus
    in the network *)

val empty_network : population -> virus -> t
(** [empty_network pop virus] is an empty network with [pop] and [virus]*)

val size : t -> int
(** [size net] is the number of people in the network*)

val fraction_infected : t -> float
(** [fraction_infected net] is the fraction of people infected in the
    network *)

val transmission_probability : t -> person_id -> person_id -> float
(** [transmission_probability net id1 id2] is the probability of virus
    transmission between person [id1] and person [id2] in network [net].
    Requires:id2 must already be infected, [id1], [id2] are people in
    the network. Raises [UnknownEdge (id1,id2)] if an edge between [id1]
    and [id2] does not exist.*)

val update_state : t -> t
(** [update_state state] is the infection network generated from [state]
    after one time. Requires: [state] is a valid infection network*)
