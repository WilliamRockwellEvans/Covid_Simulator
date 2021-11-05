module type DataType = sig
  type t
  (** [t] is the representation type for DataType
 *)
  val from_string : string -> t
  (** [from_string s] converts JSON [s] to the representation type. Raises [InvalidJSON] if the input string is not valid *)
  val pp : t -> string
  (** [pp t] pretty-prints the value of DataType *)
end

(********************** Representation types **********************)
type position = int list
(** [position] represents a person's location in network*)

type vax =
  | Two_or_more
  | One
  | Zero
(** [vax_type] is the type of someone's vaccination status*)

type status =
  | Infected
  | Dead
  | Not_infected
(** [status] is the type of people's status, ie Infected, Not infected, Dead*)

type mask =
  | Masked
  | Not_masked
(** [mask] is the representation type for mask status *)

type sociability =
  | Low
  | Medium
  | High
(** [sociability] is the rep. type for a person's sociability level*)

type interaction_time =
  | Short
  | Regular
  | Long

(** [interaction_time] represents the length of an interaction between
   nodes *)

type location =
  | Indoors
  | Outdoors
(** [location] represents the type of location a person is in*)

type density =
  | Low_density
  | Med_density
  | High_density
(** [density] represents the population density of the network*)

type incubation_time =
  | Days of int
  | Weeks of int
(** [incubation_time] represents the virus incubation time in days or weeks*)

type mortality_rate = float
(**The type of mortality rate of infected people (between 0 and 1)*)

(***************** Representation modules *************)

module Position : DataType with type t = int list

module Vaccine : DataType with type t = vax

module Infected : DataType with type t = status

module Mask : DataType with type t = mask

module Sociability : DataType with type t = sociability

module InteractionTime : DataType with type t = interaction_time

module Location : DataType with type t = location

module Density : DataType with type t = density

module IncubationTime : DataType with type t = incubation_time
