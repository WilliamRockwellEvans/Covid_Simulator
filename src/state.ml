(* let get_neighbors node edge_lst = List.map (fun (_,b) -> b)
   (List.filter (fun (a,_) -> a == node) edge_lst) *)

let rec get_info graph s acc = function
  | [] -> List.rev acc
  | h :: t ->
      let temp = Network.edge_information graph s h in
      get_info graph s (temp :: acc) t

let rec propogate_infection (attr : Network.attr) neighbors acc =
  if attr.infected = Network.Infected then
    match neighbors with
    | [] -> List.rev acc
    | h :: t -> propogate_infection attr t ((h, Network.Infected) :: acc)
  else
    match neighbors with
    | [] -> List.rev acc
    | h :: t ->
        propogate_infection attr t ((h, Network.Not_infected) :: acc)

let visited n lst = List.filter (fun (a, _) -> a = n) lst <> []

let dfs_infection (graph : Network.t) start =
  let rec dfs_aux curr_res n =
    if visited n curr_res then curr_res
      (* node's paths have already been updated *)
    else
      (* node has not been visited *)
      let neigh = Network.(n |> neighbors graph) in
      let neigh_edge_info = get_info graph n [] neigh in
      let attr = Network.(n |> get_attributes graph) in
      let new_res = propogate_infection attr neigh [] in

      (* let info = Network.edge_information n *)
      (* need to call recursively on all of the neighbors *)
      List.fold_left dfs_aux ((n, attr.infected) :: new_res) neigh
    (* List.fold_left dfs_aux res ():: visited) *)
  in

  let a =
    Network.(start |> get_attributes graph) |> fun x ->
    [ (start, x.infected) ]
  in
  dfs_aux a start

let update_state (graph : Network.t) :
    (Network.person_id * Network.infected) list =
  graph |> Network.head |> dfs_infection graph
