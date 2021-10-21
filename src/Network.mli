(**Representation of static social network.

   This module handles the data stored in the file that stores a social
   network. It includes the data of the people and their interactions.
   It handles loading of two csv files (attribute list and edge list)
   that include the initial conditions and the parameters of the
   network, as well as querying the data. The attribute list csv file
   will contain the individual parameters of the people in the network.
   The edge list csv file will contain each interaction and their
   respective parameters.*)

type t
(** The abstract type of the values representing infection network*)

type person_id = int
(** The type of person identifiers*)

type position = int list
(** The type of the position in the positive x-y plane. The first value
    of the list is the x value, the second is the y value. Precondition:
    position has exactly two elements *)

type line = position * position
(** the type of a line between two points in a grid*)

type infected =
  | Infected
  | Not_infected  (**The type of the state of infection of a person*)

type graph = {
  nodes : (position * infected) list;
  edges : line list;
}
(** The type of a graph for the network*)

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

exception UnknownPerson of person_id
(** Raised when [person_id] is not in the network*)

exception UnknownEdge of person_id * person_id
(** Raised when a non-existing edge is encountered*)

exception InvalidJSON
(** Raised when the input JSON has an invalid form*)

exception InvalidPosition of position
(** Raised when an illegal position is encountered*)

val from_json : Yojson.Basic.t -> t
(** [from_json j] is the network that [j] represents. Requires: [j] is a
    valid JSON network representation. *)

val head : t -> person_id
(**[head net] is the first person in the network*)

val people : t -> person_id list
(**[people net] is the list of person ids in the network*)

val neighbors : t -> person_id -> person_id list
(**[neighbors net p] is a set-like list of all the people who [p] has an
   interaction with in network [net]. Raises [UnknownPerson p] if the
   [p] is not a person in [net]*)

val get_position : t -> person_id -> position
(** [get_position net p] is the position of person [p] in network [net].
    Raises [UnknownPerson p] if person [p] does not exist in [net]*)

val get_attributes : t -> person_id -> attr
(** [get_attributes net p] is the attributes of person [p] in network
    [net]. Raises [UnknownPerson p] if person [p] does not exist in
    [net]*)

val edge_information : t -> person_id -> person_id -> edge_info
(** [get_edge_info net p1 p2] is the edge information of the edge
    between person [p1] and person [p2]. Raised [UnknownPerson p1] if p1
    does not exist in [net]. Raises [UnknownEdge (p1,p2)] if an edge
    between [p1] and [p2] does not exist.*)

val create_graph : t -> graph
(** [create_graph net] is the graph type representation of the network
    net to bet used in the GUI *)
