open Graphics

let from_rgb (c : Graphics.color) = 
  let r = c/65536 and g = c/256 mod 256 and b = c mod 256 in 
  (r,g,b)

let inv_color (c : Graphics.color) = 
  let (r,g,b) = from_rgb c in 
  Graphics.rgb (255 - r) (255 - g) (255 - b)

let draw_rect x0 y0 w h = 
  let (a,b) = Graphics.current_point () and 
  x1 = x0 + w and y1 = y0 + h in 
  Graphics.moveto x0 y0;
  Graphics.lineto x0 y1; Graphics.lineto x1 y1;
  Graphics.lineto x1 y0; Graphics.lineto x0 y0;
  Graphics.moveto a b;;

Graphics.open_graph "foo:0 200x300";;
draw_rect 15 15 100 100;;

let draw_poly r = 
  let (a,b) = Graphics.current_point () in 
  let (x0, y0) = r.(0) in Graphics.moveto x0 y0;
  for i = 1 to (Array.length r)-1 do 
    let (x,y) = r.(i) in Graphics.lineto x y 
  done;
  Graphics.lineto x0 y0;
  Graphics.moveto a b;;

let pi = 3.1415927;;

let net_points (x,y) l n = 
  let a = 2. *. pi /. (float n) in 
  let rec aux (xa, ya) i =
    if i > n then []
    else 
      let na = (float i ) *. a in 
      let x1 = xa + (int_of_float (cos(na) *. l))
      and y1 = ya + (int_of_float (sin(na) *. l)) in 
      let np = (x1, y1) in 
        np :: (aux np (i+1))
  in Array.of_list (aux (x,y) 1);;

let draw_net (x,y) l n sc st = 
  let r = net_points (x,y) l n in 
  draw_poly r;
  let draw_machine (x,y) = 
    Graphics.set_color Graphics.background;
    Graphics.fill_circle x y sc; 
    Graphics.set_color Graphics.foreground;
    Graphics.draw_circle x y sc;
  in 
  Array.iter draw_machine r;
  Graphics.fill_circle x y st;;

draw_net (140, 20) 60.0 10 10 3;;


let () = print_endline "Hello, World!"
