open Graphics
open Covid
open Network

module Gui = struct
  let from_rgb (c : Graphics.color) =
    let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
    (r, g, b)

  let inv_color (c : Graphics.color) =
    let r, g, b = from_rgb c in
    Graphics.rgb (255 - r) (255 - g) (255 - b)

  let draw_rect x0 y0 w h =
    let a, b = Graphics.current_point ()
    and x1 = x0 + w
    and y1 = y0 + h in
    Graphics.moveto x0 y0;
    Graphics.lineto x0 y1;
    Graphics.lineto x1 y1;
    Graphics.lineto x1 y0;
    Graphics.lineto x0 y0;
    Graphics.moveto a b

  let draw_poly r : unit =
    let a, b = Graphics.current_point () in
    let x0, y0 = r.(0) in
    Graphics.moveto x0 y0;
    for i = 1 to Array.length r - 1 do
      let x, y = r.(i) in
      Graphics.lineto x y
    done;
    Graphics.lineto x0 y0;
    Graphics.moveto a b

  let net_points (x, y) l n =
    let a = 2. *. Float.pi /. float n in
    let rec aux (xa, ya) i =
      if i > n then []
      else
        let na = float i *. a in
        let x1 = xa + int_of_float (cos na *. l)
        and y1 = ya + int_of_float (sin na *. l) in
        let np = (x1, y1) in
        np :: aux np (i + 1)
    in
    Array.of_list (aux (x, y) 1)

  let draw_net (x, y) l n sc st : unit =
    let r = net_points (x, y) l n in
    draw_poly r;
    let draw_machine (x, y) : unit =
      Graphics.set_color Graphics.background;
      Graphics.fill_circle x y sc;
      Graphics.set_color Graphics.foreground;
      Graphics.draw_circle x y sc
    in
    Array.iter draw_machine r;
    Graphics.fill_circle x y st

  let color_of_state = function
    | Infected -> Graphics.red
    | Not_infected -> Graphics.blue

  let rec draw_person_lst r = function
    | [] -> ()
    | (x, y, st) :: t ->
        st |> color_of_state |> Graphics.set_color;
        Graphics.fill_circle x y r;
        Graphics.set_color Graphics.foreground;
        Graphics.draw_circle x y r;

        draw_person_lst r t

  let rec draw_edges_lst = function
    | [] -> ()
    | ((x1, y1), (x2, y2)) :: t ->
        Graphics.moveto x1 y1;
        Graphics.lineto x2 y2;
        draw_edges_lst t

  let get_person_net point_lst line_lst =
    let a, b = Graphics.current_point () in
    draw_edges_lst line_lst;
    draw_person_lst 10 point_lst;
    Graphics.moveto a b

  let person_of_graph = function
    | [ x; y ], i -> (100 * x, 100 * y, i)
    | _ -> failwith "bad"

  let edge_of_graph = function
    | [ x1; y1 ], [ x2; y2 ] ->
        ((100 * x1, 100 * y1), (100 * x2, 100 * y2))
    | _ -> failwith "bad"

  let unpackage_graph file = file |> Yojson.Basic.from_file |> from_json

  let graph_of_json file = file |> unpackage_graph |> create_graph

  let nodes_of_graph g = g.nodes

  let edges_of_graph g = g.edges

  let write_title () =
    let a, b = Graphics.current_point () in
    let x_tot = Graphics.size_x () in
    let y_tot = Graphics.size_y () in
    let title = "Covid Network" in
    Graphics.moveto (x_tot / 2) (8 * y_tot / 9);
    Graphics.draw_string title;
    Graphics.moveto a b

  let get_person graph =
    graph |> nodes_of_graph |> List.map person_of_graph

  let get_edges graph =
    graph |> edges_of_graph |> List.map edge_of_graph

  let rec time_update t graph =
    match t with
    | i when i = 0 -> graph
    | j ->
        let updated = graph |> State.update_state in
        let replacement = create_graph updated in
        let () =
          get_person_net
            (replacement |> get_person)
            (replacement |> get_edges)
        in
        time_update (t - 1) updated

  let is_digit_char c =
    let i = Char.code c in
    i - 48 <= 9 && i - 48 >= 0

  let get_time c =
    if is_digit_char c then Char.code c - 48
    else failwith "Not a number input"

  let is_escape_key c = c |> Char.code |> fun x -> x = 27

  let is_on_gui x y =
    x < Graphics.size_x () && x >= 0 && y < Graphics.size_y () && y >= 0

  let get_person_click status =
    let x, y = (status.mouse_x, status.mouse_y) in
    if is_on_gui x y then (x, y) else (~-1, ~-1)

  let extract_pos i =
    match i with
    | [ x_p; y_p ], infected -> (x_p, y_p)
    | _ -> failwith "Should not happen"

  let rec get_person_pos_lst acc = function
    | [] -> List.rev acc
    | h :: t ->
        let x, y = extract_pos h in
        get_person_pos_lst ((x, y) :: acc) t

  let rec person_id_of_positionAUX x y acc_id = function
    | [] -> failwith "Should not happen"
    | h :: t ->
        let x_p, y_p = h in
        if (x_p, y_p) = (x, y) then acc_id
        else person_id_of_positionAUX x y (acc_id + 1) t

  let rec person_id_of_position person_lst x y =
    person_id_of_positionAUX x y 1 person_lst

  let get_infection = function
    | Network.Infected -> "Infected"
    | Network.Not_infected -> "Not Infected"

  let get_sociability = function
    | Network.Low -> "Low"
    | Network.High -> "High"
    | Network.Medium -> "Medium"

  let get_masked = function
    | Network.Masked -> "Masked"
    | Network.Not_masked -> "Not Masked"

  let get_vaccination = function
    | Network.Two_or_more -> "Completed both vaccinations"
    | Network.One -> "One vaccination completed"
    | Network.Zero -> "No vaccinations taken"

  let move_down_10 () =
    let x_len, y_len = (Graphics.size_x (), Graphics.size_y ()) in
    Graphics.moveto (10 * x_len / 11) (10 * y_len / 11)

  let in_circle x y x_c y_c r =
    ((float x -. float x_c) ** 2.) +. ((float y -. float y_c) ** 2.)
    <= float r ** 2.

  let rec is_in_node status people rad g =
    let x, y = get_person_click status in
    if (x, y) = (~-1, ~-1) then ()
    else
      match people with
      | [] -> ()
      | h :: t ->
          let x_c, y_c, infection = h in
          if in_circle x y x_c y_c rad then begin
            (* clicked on this person's node in graph *)
            let graph = g |> create_graph in
            let ppl =
              graph |> nodes_of_graph |> get_person_pos_lst []
            in
            let id = person_id_of_position ppl x_c y_c in
            let attr = Network.get_attributes g id in

            move_down_10 ();
            attr.infected |> get_infection |> ( ^ ) "Infection: "
            |> Graphics.draw_string;

            move_down_10 ();
            attr.sociability |> get_sociability |> ( ^ ) "Sociability: "
            |> Graphics.draw_string;

            move_down_10 ();
            attr.mask |> get_masked |> ( ^ ) "Masked: "
            |> Graphics.draw_string;

            move_down_10 ();
            attr.vaccine_doses |> get_vaccination
            |> ( ^ ) "Vaccination: " |> Graphics.draw_string
          end
          else is_in_node status t rad g

  let rec update_status graph stat =
    if stat.keypressed then
      if stat.key |> is_escape_key then Graphics.close_graph ()
      else if stat.key |> is_digit_char then
        let updated = time_update (stat.key |> get_time) graph in
        update_status updated stat
      else if stat.button then
        is_in_node stat (graph |> create_graph |> get_person) 100 graph
end

let events =
  [
    Graphics.Key_pressed;
    Graphics.Button_down;
    Graphics.Button_up;
    Graphics.Poll;
  ]

let g = Gui.unpackage_graph "data/basic_network_stepped.json"

let stepped_graph =
  "data/basic_network_stepped.json" |> Gui.graph_of_json

let ppl = stepped_graph |> Gui.get_person

let edges = stepped_graph |> Gui.get_edges

let () = Graphics.open_graph " 700x700";;

Gui.get_person_net ppl edges;
Gui.write_title ();

let _ = Graphics.loop_at_exit events (Gui.update_status g) in

()
