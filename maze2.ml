
(* compile with: ocamlopt -o maze.exe graphics.cmxa unix.cmxa maze.ml
 * 
 * http://www.conwaylife.com/w/index.php?title=Maze 
 *)

let artifex = "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI";;

let version = "2.4";;

let verbose = ref false;;

open Graphics;;

let wrap x y =
  if x < 0 then x + y
  else if x >= y then x - y
  else x;;
  
let gen m1 m2 color min_neighbours max_neighbours min_birth max_birth =
  let xdim = Array.length m1 in
  let ydim = Array.length m1.(0) in
  let xmax = xdim - 1 in
  let ymax = ydim - 1 in
  for x = 0 to xmax do
    for y = 0 to ymax do
      let k = ref 0 in
      for dx = -1 to 1 do
        for dy = -1 to 1 do
          if dx <> 0 || dy <> 0 then
            let xx = wrap (x + dx) xdim in
            let yy = wrap (y + dy) ydim in
            if m1.(xx).(yy) <> white then
              incr k
        done
      done;
      let color0 = m1.(x).(y) in
      if color0 <> white then begin
        if !k >= min_neighbours && !k <= max_neighbours then begin
            m2.(x).(y) <- color0
        end else begin
            m2.(x).(y) <- white
        end;
      end else if !k >= min_birth && !k <= max_birth then
            m2.(x).(y) <- color
      else m2.(x).(y) <-  white
    done
  done;;
  
let init m radius init_probability =
  let xdim = Array.length m in
  let ydim = Array.length m.(0) in
  let x0 = xdim / 2 in
  let y0 = ydim / 2 in
  for x = max (x0 - radius) 0 to min (xdim - 1) (x0 + radius) do
    for y = max (y0 - radius) 0 to min (ydim - 1) (y0 + radius) do
      if Random.float 1.0 < init_probability then
        m.(x).(y) <- black
    done
  done;;

let display m enlargement air =
  let enlargement' = enlargement - air in
  let xmax = Array.length m - 1 in
  let ymax = Array.length m.(0) - 1 in
  for x = 0 to xmax do
    let col = m.(x) in
    for y = 0 to ymax do
      set_color col.(y);
      fill_rect (x * enlargement) (y * enlargement) enlargement' enlargement'
    done
  done;
  synchronize ();;
  
let incr_index bounds array =
  let rec loop i =
    if i < 0 then
      raise Exit
    else
      let x = array.(i) in
      if x >= bounds.(i) - 1 then begin
        array.(i) <- 0;
        loop (pred i)
      end else begin
        array.(i) <- succ(x)
      end
  in loop (Array.length array - 1);;

let vec_white = Array.make 3 255;;

let color_init k =
  for i = 0 to 2 do
    k.(i) <- 0
  done;;

let choose_color k color_step =
  let ss = 256 / color_step in
  let bounds = Array.make 3 color_step in
  (try incr_index bounds k
   with Exit -> color_init k);
  let color0 = k.(0) * ss * 65536 +
                 k.(1) * ss * 256 +
                 k.(2) * ss in
  let color = if color0 = white then black
              else color0 in
  if !verbose then
    Printf.printf "choose_color: chosen = 0x%.6x (%d, %d, %d)\n"
                  color k.(0) k.(1) k.(2);
  color;;

let nap s =
  ignore (Unix.select [] [] [] (max 0.0 s));;

let swap ref1 ref2 =
  let temp = !ref1 in
  ref1 := !ref2;
  ref2 := temp;;
  
let maze xdim ydim
         min_neighbours max_neighbours
         min_birth max_birth
         enlargement air
         color_step
         radius init_probability
         framerate_limitator =
  open_graph (Printf.sprintf " %dx%d" (xdim * enlargement + 20) (ydim * enlargement + 50));
  set_window_title (Printf.sprintf "Maze %s" version);
  (* resize_window (xdim * enlargement) (ydim * enlargement); *)
  auto_synchronize false;
  display_mode false;
  let m1 = ref (Array.make_matrix xdim ydim white) in
  let m2 = ref (Array.make_matrix xdim ydim white) in
  init !m1 radius init_probability;
  let gen_counter = ref 0 in
  let color = Array.make 3 0 in
  let time_per_frame =
    match framerate_limitator with
    | None -> 0.0
    | Some fps -> 1.0 /. fps in
  let napped = ref 0.0 in
  let t0 = Sys.time () in
  while true do
    let t1 = Sys.time () in
    if !verbose then begin
      let dt = t1 -. t0 in
      Printf.printf
        "gen/time = %d/(%.1f s) = %.1f Hz; nap/total = (%.1f s)/(%.1f s) = %.1f%%\n%!"
        !gen_counter dt ((float !gen_counter) /. dt)
        !napped dt
        (!napped /. dt *. 100.0)
      end;
    incr gen_counter;
    display !m2 enlargement air;
    gen !m1 !m2 (choose_color color color_step) min_neighbours max_neighbours min_birth max_birth;
    swap m1 m2;
    let tz = t1 +. time_per_frame in
    let ty = Sys.time () in
    let dt = tz -. ty in
    if dt > 0.0 then begin
      nap dt;
      napped := !napped +. dt
    end
  done;;

let user_manual =
  Printf.sprintf "Use: %s [xdim] [ydim]\nUse: %s -help\n" Sys.argv.(0) Sys.argv.(0);;

let print_user_manual_and_die () =
  print_endline user_manual;
  if not !Sys.interactive then
    exit 1;;

let main () =
  let anons = ref [] in
  let min_neighbours = ref 1 in
  let max_neighbours = ref 5 in
  let min_birth = ref 3 in
  let max_birth = ref 3 in
  let radius = ref 5 in
  let init_probability = ref 0.5 in
  let air = ref 1 in
  let enlargement = ref 4 in
  let color_step = ref 8 in
  let framerate_limitator = ref None in
  let user_seed = ref None in
  (try
     Arg.parse
       [("-enl", Arg.Int (fun i -> enlargement := i), "enlargement");
        ("-step", Arg.Int (fun i -> color_step := i), "color step");
        ("-min", Arg.Int (fun i -> min_neighbours := i), "min live neighbours for surviving");
        ("-man", Arg.Int (fun i -> max_neighbours := i), "max live neighbours for surviving");
        ("-mib", Arg.Int (fun i -> min_birth := i), "min live neighbours for birth");
        ("-mab", Arg.Int (fun i -> max_birth := i), "max live neighbours for birth");
        ("-rad", Arg.Int (fun i -> radius := i), "radius of random initial square");
        ("-air", Arg.Int (fun i -> air := i), "empty space between cells");
        ("-prob", Arg.Float (fun x -> init_probability := x),
         "probability of live cell in the initialized square");
        ("-life", Arg.Unit (fun () ->
                      min_neighbours := 2;
                      max_neighbours := 3;
                      min_birth := 3;
                      max_birth := 3;
                      radius := 99999999), "set default parameters for life");
        ("-maze", Arg.Unit (fun () ->
                      min_neighbours := 1;
                      max_neighbours := 5;
                      min_birth := 3;
                      max_birth := 3;
                      radius := 5), "set default parameters for maze");
        ("-verbose", Arg.Unit (fun () -> verbose := not !verbose), "flip verbose mode");
        ("-fps", Arg.Float (fun s -> framerate_limitator := Some s), "max framerate");
        ("-seed", Arg.Int (fun s -> user_seed := Some s), "random generator seed");
       ]
       (fun anon -> anons := int_of_string anon :: !anons)
       user_manual
   with
     Failure mess -> print_user_manual_and_die ());
  
  let start xdim ydim =
    let seed = match !user_seed with
      | None ->
         Random.self_init ();
         Random.bits ()
     | Some number ->
        number
    in
    Random.init seed;
    Printf.printf "-seed %d%!" seed;
    maze xdim ydim
         !min_neighbours !max_neighbours
         !min_birth !max_birth
         !enlargement !air
         !color_step
         !radius !init_probability
         !framerate_limitator
  in
  match !anons with
  | [y; x] -> start x y
  | [x] -> start x x
  | [] -> start 128 128
  | _ -> print_user_manual_and_die ();;

if not !Sys.interactive then
  main ();;
