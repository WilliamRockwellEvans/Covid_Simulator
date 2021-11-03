type t = Network.t

module PersonKey = struct
  type t = Network.person_id

  let compare = Stdlib.compare
end

module NodeSet = Set.Make (PersonKey)

(* [visitedTracker] implements a logger for tracking which nodes have
   been visited during a graph traversal*)
class visitedTracker =
  object (self)
    val mutable visited = NodeSet.empty (* can i make this private?*)

    (* [add_visit n] updates tracker with the fact that node [n] has
       been visited *)
    method add_visit n = visited <- NodeSet.add n visited

    (* [was_visited n] is if node [n] has been visited before*)
    method was_visited (n : Network.person_id) : bool =
      NodeSet.mem n visited

    (* [clear] deletes all entries from the tracker *)
    method clear = visited <- NodeSet.empty
  end

(* [infection_prob node neighbor] is the likelihood of [node] being
   infected by [neighbor]*)
let infection_prob (n : Network.person_id) neighbor = 1.

(* [rand_bool prob] generates [true] with probability [prob], otherwise
   [false] *)
let rand_bool prob : bool = Random.float 1. < prob

(* [process_node state n] is if node [n] in state [state] gets infected
   by its neighbors *)
let process_node state (n : Network.person_id) =
  let rec lst_itr acc = function
    | h :: t -> lst_itr (infection_prob n h :: acc) t
    | [] -> acc
  in
  lst_itr []
    (Network.neighbors state n
    |> List.filter (fun neighbor ->
           (Network.get_attributes state neighbor).infected = Infected)
    )
  |> List.map rand_bool
  |> List.exists (fun x -> x)

(* [generate_updates state tracker] is a map of people in state who get
   infected*)
let generate_updates state tracker =
  let rec generate_rec (node : Network.person_id) inf_acc =
    tracker#add_visit node;
    let new_inf_acc =
      if process_node state node then NodeSet.add node inf_acc
      else inf_acc
    in
    let rec lst_itr = function
      | [] -> new_inf_acc
      | h :: t ->
          if tracker#was_visited h then lst_itr t
          else NodeSet.union (generate_rec h new_inf_acc) (lst_itr t)
    in
    lst_itr (Network.neighbors state node)
  in
  generate_rec (Network.head state) NodeSet.empty

(* [apply_changes state changes] parses through the graph and applies
   [changes]*)
let rec apply_changes (state : Network.t) changes =
  let rec iterate_people net_acc = function
    | [] -> net_acc
    | h :: t ->
        let pers = Network.get_person state h in
        let new_person =
          if NodeSet.mem h changes then
            {
              pers with
              attributes = { pers.attributes with infected = Infected };
            }
          else pers
        in
        iterate_people
          (Network.add_person net_acc h new_person.attributes
             new_person.neighbors)
          t
  in
  iterate_people Network.empty_network (Network.people state)

let update_state state =
  generate_updates state (new visitedTracker) |> apply_changes state
