type t = Network.t

module PersonKey = struct
  type t = Network.person_id

  let compare = Stdlib.compare
end

module NodeMap = Map.Make (PersonKey)

(* [visitedTracker] implements a logger for tracking which nodes have
   been visited during a graph traversal*)
class visitedTracker =
  object (self)
    val mutable map = NodeMap.empty (* can i make this private?*)

    (* [add_visit n] updates tracker with the fact that node [n] has
       been visited *)
    method add_visit n = map <- NodeMap.add n true map

    (* [was_visited n] is if node [n] has been visited before*)
    method was_visited (n : Network.person_id) : bool =
      NodeMap.mem n map

    (* [clear] deletes all entries from the tracker *)
    method clear = map <- NodeMap.empty
  end

let not_yet_visited tracker n = not (tracker#was_visited n)

(* [infection_prob node neighbor] is the likelihood of [node] being
   infected by [neighbor]*)
let infection_prob (n : Network.person_id) neighbor = 1.

(* [rand_bool prob] generates a boolean with [True] having probability
   [prob] *)
let rand_bool prob : bool = prob <= Random.float 1.

(* [process_node state n] is whether node [n] in state [state] gets
   infected by its neighbors *)
let process_node state (n : Network.person_id) =
  let rec lst_itr acc = function
    | h :: t -> lst_itr (infection_prob n h :: acc) t
    | [] -> acc
  in
  lst_itr [] (Network.neighbors state n)
  |> List.map rand_bool
  |> List.exists (fun x -> x)

let rec generate_tr acc state tracker n =
  tracker#add_visit n;
  let rec lst_itr unvisited_neighbors =
    match unvisited_neighbors with
    | [] -> acc
    | h :: t -> generate_tr (h :: lst_itr t) state tracker h
  in
  let processed_neighbors =
    lst_itr
      (Network.neighbors state n
      |> List.filter (not_yet_visited tracker))
  in
  if process_node state n then n :: processed_neighbors
  else processed_neighbors

(* [generate_updates tracker n] is the list of updates to be applied to
   the graph*)
let rec generate_updates state tracker n =
  generate_tr [] state tracker n

(* [apply_changes state changes] parses through the graph and applies
   [changes]*)
let rec apply_changes state tracker changes =
  tracker#clear;
  failwith "Failure: Unimplemented"

let update_state state = state
(* let the_tracker = new visitedTracker in Network.head state |>
   generate_updates state the_tracker |> apply_changes state
   the_tracker *)