open Graphics
open Covid
open Network

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

let graph_of_json file =
  file |> Yojson.Basic.from_file |> from_json |> create_graph

let nodes_of_graph g = g.nodes

let edges_of_graph g = g.edges

let stepped_graph = "basic_network_stepped.json" |> graph_of_json

let ppl = stepped_graph |> nodes_of_graph |> List.map person_of_graph

let edges = stepped_graph |> edges_of_graph |> List.map edge_of_graph

let write_title () =
  let a, b = Graphics.current_point () in
  let x_tot = Graphics.size_x () in
  let y_tot = Graphics.size_y () in
  let title = "Covid Network" in
  Graphics.moveto (x_tot / 2) (8 * y_tot / 9);
  Graphics.draw_string title;
  Graphics.moveto a b

let () = Graphics.open_graph " 700x700";;

write_title ()
