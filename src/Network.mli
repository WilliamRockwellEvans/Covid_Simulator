(*Representation of static social network. 

  This module handles the data stored in the file that stores a social network.
  It includes the data of the people and their interactions. It handles loading
  of two csv files (attribute list and edge list) that include the initial 
  conditions and the parameters of the network, as well as querying the data. 
  The attribute list csv file will contain the individual parameters of the 
  people in the network. 
  The edge list csv file will contain each interaction and their respective 
  parameters.*)

  type t 
  (** The abstract type of the values representing infection network*)

  type person_id = int
  (** The type of person identifiers*)

  type person_attr = {id : person_id ;
                    infected : char ;
                    mask : char ;
                    immunity : float }
  (** The type of individual attributes of person [id]*)

  type edge = person_id * person_id
  (** The type of edge between two persons. 
  Note: edge is undirected*)

  type edge_info = {edge : edge;
                    distance : float;
                    risk : string} 
  (** The type of edge info of an edge [edge].*)

  type position = int * int
  (** The type of the position in the positive x-y plane. 
  The first value of the pair is the x value, the second is the y value.*)

  exception UnknownPerson of person_id
  (** Raised when an unknownperson is encountered*)

  exception InvalidPosition of position
  (** Raised when an illegal position is encountered*)

  val attributes_from_csv : 
  
  val interactions_from_csv : 

  val edges : t -> person_id -> person_id list
  (**[edges net p] is a set-like list of all the people who [p] has an 
  interaction with in network [net]. Raises [UnknownPerson p] if the [p]
  is not a person in [net]*)

  val get_postiion : t -> person_id -> position
  (** [get_position net p] is the position of person [p] 
  in network [net]. Raises [UnknownPerson p] if person [p] 
  does not exist in [net]*)
  
  val attributes : t -> person_id -> person_attr
  (** attributes net p] is the attributes of person [p]
  in network [net]. Raises [UnknownPerson p] if person [p] 
   does not exist in [net]*)

  val edge_information : t -> edge -> edge_info
  (** [get_edge_info net p1 p2] is the edge information of the edge
  between person [p1] and person [p2] *)