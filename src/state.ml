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

(* [infection_prob node neighbor] is the likelihood of [node] being
   infected by [neighbor]*)
let infection_prob (n : Network.person_id) neighbor = 1.

(* [rand_bool prob] generates a boolean with [True] having probability
   [prob] *)
let rand_bool prob : bool = prob <= Random.float 1.

(* [process_node state n] is if node [n] in state [state] gets infected
   by its neighbors *)
let process_node state (n : Network.person_id) =
  let rec lst_itr acc = function
    | h :: t -> lst_itr (infection_prob n h :: acc) t
    | [] -> acc
  in
  lst_itr [] (Network.neighbors n state)
  |> List.map rand_bool
  |> List.exists (fun x -> x)

(* [generate_updates state tracker] is the list of people in state who
   get infected*)
let generate_updates state tracker =
  let rec generate_rec
      (node : Network.person_id)
      (inf_acc : Network.person_id list) =
    tracker#add_visit node;
    let new_inf_acc =
      if process_node state node then node :: inf_acc else inf_acc
    in
    let rec lst_itr = function
      | [] -> []
      | h :: t ->
          if tracker#was_visited h then lst_itr t
          else generate_rec h new_inf_acc
    in
    lst_itr (Network.neighbors node state)
  in
  generate_rec (Network.head state) []

(* [apply_changes state changes] parses through the graph and applies
   [changes]*)
let rec apply_changes state tracker changes = tracker#clear

let update_state (state : t) : t = state
(* let the_tracker = new visitedTracker in Network.head state |>
   generate_updates state the_tracker |> apply_changes state
   the_tracker *)
