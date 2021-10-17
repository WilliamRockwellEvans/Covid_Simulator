type wellbeing =
  | Healthy
  | Sick

type node = {
  id : string;
  neighbors : node list;
  status : wellbeing;
  mutable processed : bool;
}

module Node = struct
  type t = node

  let compare a b = Stdlib.compare a.id b.id
end

module NodeMap = Map.Make (Node)

module Graph = struct
  let t =
    NodeMap.(
      empty
      |> add
           {
             id = "hi";
             neighbors = [];
             status = Healthy;
             processed = false;
           }
           0)

  let head state =
    { id = "hi"; neighbors = []; status = Healthy; processed = false }

  let find_node n state =
    if NodeMap.mem n state then Some (NodeMap.find n state) else None

  let list_nodes state = []
end

(** [infection_prob node neighbor] is the likelihood of [node] being
    infected by [neighbor]*)
let infection_prob node neighbor = 1.

(** [rand_bool prob] generates a boolean with [True] having probability
    [prob] *)
let rand_bool prob : bool = prob <= Random.float 1.

let not_visited n = n.processed

(** [process_node n] is whether [n] gets infected by its neighbors *)
let process_node n =
  let rec lst_itr acc = function
    | h :: t -> lst_itr (infection_prob n h :: acc) t
    | [] -> acc
  in
  lst_itr [] n.neighbors |> List.map rand_bool
  |> List.exists (fun x -> x)

let rec generate_updates acc n =
  n.processed <- true;
  (* Sets this node to having been visited*)
  let rec lst_itr unvisited_neighbors =
    match unvisited_neighbors with
    | [] -> []
    | h :: t -> generate_updates [] h @ lst_itr t
  in
  if process_node n then
    n :: lst_itr (n.neighbors |> List.filter not_visited)
  else lst_itr (n.neighbors |> List.filter not_visited)

let rec apply_changes state changes = failwith "Failure: unimplemented"

let update_state state =
  state |> Graph.head |> generate_updates [] |> apply_changes state

(* module VisitedTracker : sig type t (** The abstract type for the
   VisitedTracker*) val init : t (** [init] creates a new Tracker with
   no logs*) val add_visit : Node.t -> t (** [add_visit node t] updates
   tracker [t] with the fact that [node] has been visited *) val
   was_visited : Node.t -> bool (** [was_visited node] is if node has
   been visited before*) end = struct type t = node let init =
   NodeMap.empty let add_visit node track= NodeMap.add node true track
   let was_visited node track = NodeMap.mem node track end (**
   [VisitedTracker] implements a logger for tracking which nodes have
   been visited during a graph traversal*) *)
