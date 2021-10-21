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
  lst_itr [] (Network.neighbors state n)
  |> List.map rand_bool
  |> List.exists (fun x -> x)

(* [generate_updates state tracker] is a map of people in state who get
   infected*)
let generate_updates state tracker =
  let rec generate_rec (node : Network.person_id) inf_acc =
    tracker#add_visit node;
    let new_inf_acc =
      if process_node state node then NodeMap.add node true inf_acc
      else inf_acc
    in
    let rec lst_itr = function
      | [] -> NodeMap.empty
      | h :: t ->
          if tracker#was_visited h then lst_itr t
          else
            NodeMap.merge
              (fun h o1 o2 ->
                match (o1, o2) with
                | None, None -> None
                | None, Some v | Some v, None -> Some v
                | _, _ -> failwith "Invalid arg: maps aren't disjoint")
              inf_acc
              (generate_rec h new_inf_acc)
    in
    lst_itr (Network.neighbors state node)
  in
  generate_rec (Network.head state) NodeMap.empty

(* [apply_changes state changes] parses through the graph and applies
   [changes]*)
let rec apply_changes (state : Network.t) changes =
  let rec iterate_people net_acc = function
    | [] -> net_acc
    | h :: t ->
        let pers = Network.get_person state h in
        let new_acc =
          if NodeMap.mem h changes then
            Network.add_person net_acc h
              { pers.attributes with infected = Infected }
              pers.neighbors
          else
            Network.add_person net_acc h pers.attributes pers.neighbors
        in
        iterate_people new_acc t
  in
  iterate_people Network.empty_network (Network.list_people state)

let update_state state =
  generate_updates state (new visitedTracker) |> apply_changes state
