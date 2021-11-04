module type DataType = sig
  type t

  val from_string : string -> t

  val pp : t -> string
end

(***********************Personal Parameters******************)
type position = int list

type vax =
  | Two_or_more
  | One
  | Zero
(* [vax_type] is the type of someone's vaccination status*)

type infect =
  | Infected
  | Not_infected
(* [infection] is the type of people's infection status*)

type mask =
  | Masked
  | Not_masked
(* [mask] is the representation type for mask status *)

type sociability =
  | Low
  | Medium
  | High
(* [sociability] is the rep. type for a person's sociability level*)

type interaction_time =
  | Short
  | Regular
  | Long

(* [interaction_time] represents the length of an interaction between
   nodes *)
(*********************End Personal parameters********************
  ********************Start Population Parameters*******************)
type location =
  | Indoors
  | Outdoors
(*The type of the location of the network*)

type density =
  | Low_density
  | Med_density
  | High_density
(*The type of the population density of the network*)

(****************** End Population parameters ********************
  **************** Start Virus Parameters *********************)

type incubation_time =
  | Days of int
  | Weeks of int
(*The type of incubation time in either units of days or weeks*)

type mortality_rate = float
(**The type of mortality rate of infected people (between 0 and 1)*)

(******************** End Virus Parameters *******************)

(********************* Start Basic Type Modules **************)

module Position : DataType with type t = position = struct
  type t = position

  let from_string (a : string) =
    ignore (raise (Errors.InvalidPosition []));
    []

  let pp pos =
    Printf.sprintf "(%i, %i)" (List.hd pos) (List.hd (List.tl pos))
end

module Vaccine : DataType with type t = vax = struct
  type t = vax

  let int_to_vaccine_doses = function
    | i when i >= 2 -> Two_or_more
    | i when i = 1 -> One
    | i when i = 0 -> Zero
    | i ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf
                {|"%i" is not a valid vaccination count integer|} i))

  let from_string n = n |> int_of_string |> int_to_vaccine_doses

  let pp = function Two_or_more -> "2" | One -> "1" | Zero -> "0"
end

module Infected : DataType with type t = infect = struct
  type t = infect

  let from_string = function
    | s when s = "yes" -> Infected
    | s when s = "no" -> Not_infected
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf {|"%s" is not a valid infection string|} s))

  let pp = function
    | Infected -> "infected"
    | Not_infected -> "not infected"
end

module Mask : DataType with type t = mask = struct
  type t = mask

  let from_string = function
    | s when s = "yes" -> Masked
    | s when s = "no" -> Not_masked
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf {|"%s" is not a valid mask string|} s))

  let pp = function Masked -> "masked" | Not_masked -> "no mask"
end

module Sociability : DataType with type t = sociability = struct
  type t = sociability

  let from_string = function
    | s when s = "low" -> Low
    | s when s = "high" -> High
    | s when s = "medium" -> Medium
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf {|"%s" is not a valid sociability value|} s))

  let pp = function Low -> "low" | Medium -> "medium" | High -> "high"
end

module InteractionTime : DataType with type t = interaction_time =
struct
  type t = interaction_time

  let from_string = function
    | s when s = "long" -> Long
    | s when s = "short" -> Short
    | s when s = "regular" -> Regular
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf
                {|"%s" is not a valid interaction time string|} s))

  let pp = function
    | Short -> "short"
    | Regular -> "regular"
    | Long -> "long"
end

module Location : DataType with type t = location = struct
  type t = location

  let from_string = function
    | s when s = "indoors" -> Indoors
    | s when s = "outdoors" -> Outdoors
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf {|"%s" is not a valid location string|} s))

  let pp = function Indoors -> "indoors" | Outdoors -> "outdoors"
end

module Density : DataType with type t = density = struct
  type t = density

  let from_string = function
    | s when s = "high" -> High_density
    | s when s = "low" -> Low_density
    | s when s = "medium" -> Med_density
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf {|"%s" is not a valid density string|} s))

  let pp = function
    | High_density -> "high"
    | Med_density -> "med"
    | Low_density -> "low"
end

module IncubationTime : DataType with type t = incubation_time = struct
  type t = incubation_time

  let from_string = function
    | s when String.sub s 2 (String.length s - 2) = "days" ->
        Days (int_of_string (String.sub s 0 1))
    | s when String.sub s 2 (String.length s - 2) = "weeks" ->
        Weeks (int_of_string (String.sub s 0 1))
    | s ->
        raise
          (Errors.InvalidJSON
             (Printf.sprintf
                {|"%s" is not a valid incubation time string|} s))

  let pp = function
    | Weeks w -> Printf.sprintf "%i weeks" w
    | Days d -> Printf.sprintf "%i days" d
end
