
(* compile with: ocamlopt -o maze.exe graphics.cmxa unix.cmxa maze.ml
 * 
 * http://www.conwaylife.com/w/index.php?title=Maze 
 *)

let artifex = "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI";;

let version = "2.4";;

let need_to_work_around_minimize_bug = true;;

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
  let k = ref 0 in
  for x = 0 to xmax do
    for y = 0 to ymax do
      k := 0;
      for dx = -1 to 1 do
        let xx = wrap (x + dx) xdim in
        let col = m1.(xx) in
        for dy = -1 to 1 do
          if dx <> 0 || dy <> 0 then
            let yy = wrap (y + dy) ydim in
            if col.(yy) <> white then
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
      end else if !k >= min_birth && !k <= max_birth then begin
          m2.(x).(y) <- color
      end else begin
          m2.(x).(y) <-  white
      end
    done
  done;;
  
let init m radius shape init_probability =
  let xdim = Array.length m in
  let ydim = Array.length m.(0) in
  let x0 = xdim / 2 in
  let y0 = ydim / 2 in
  let radius2 = radius * radius in
  for x = max (x0 - radius) 0 to min (xdim - 1) (x0 + radius) do
    for y = max (y0 - radius) 0 to min (ydim - 1) (y0 + radius) do
      let dx = x - x0 in
      let dy = y - y0 in
      if shape = `Square || shape = `Circle && dx * dx + dy * dy <= radius2 then
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
  done;;
  
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
  let color0 = rgb (k.(0) * ss) (k.(1) * ss) (k.(2) * ss) in
  let color = if color0 = white then black
              else color0 in
  if !verbose then
    Printf.printf "choose_color: chosen = 0x%.6x (%d, %d, %d)\n"
                  color k.(0) k.(1) k.(2);
  color;;
  
let nap s =
  ignore (Unix.select [] [] [] (max 0.0 s));;
  
let handle_keyboard () =
  if key_pressed () then begin
    ignore (read_key ());
    exit 0
  end;;
  
let swap ref1 ref2 =
  let temp = !ref1 in
  ref1 := !ref2;
  ref2 := temp;;
  
let maze xdim ydim
         min_neighbours max_neighbours
         min_birth max_birth
         enlargement air
         color_step
         radius shape init_probability
         framerate_limitator
         timekeeping =
  open_graph (Printf.sprintf " %dx%d" (xdim * enlargement + 20) (ydim * enlargement + 50));
  set_window_title (Printf.sprintf "Maze %s" version);
  (* resize_window (xdim * enlargement) (ydim * enlargement); *)
  auto_synchronize false;
  display_mode false;
  let m1 = ref (Array.make_matrix xdim ydim white) in
  let m2 = ref (Array.make_matrix xdim ydim white) in
  init !m1 radius shape init_probability;
  let color = Array.make 3 0 in
  let time_per_frame, user_request_as_verbose_info, is_fps_requested =
    match framerate_limitator with
    | None -> 0.0, "", false
    | Some fps -> 1.0 /. fps, (Printf.sprintf " (%.2f Hz requested)" fps), true
  in
  let napped = ref 0.0 in
  let t0 = Sys.time () in
  let rec run gen_counter =
    let t1 = Sys.time () in
    if need_to_work_around_minimize_bug then begin
      auto_synchronize false;
      display_mode false;
    end;
    synchronize ();
    if !verbose then begin
      let dt = t1 -. t0 in
      Printf.printf "gen/time = %d/(%.2f s) = %.2f Hz%s\n"
                    gen_counter dt ((float gen_counter) /. dt)
                    user_request_as_verbose_info;
      if is_fps_requested then begin
          Printf.printf "nap/total = (%.2f s)/(%.2f s) = %.2f%%\n"
                        !napped dt
                        (!napped /. dt *. 100.0)
      end;
      flush stdout;  
    end;
    handle_keyboard ();
    display !m2 enlargement air;
    gen !m1 !m2 (choose_color color color_step) min_neighbours max_neighbours min_birth max_birth;
    swap m1 m2;
    let gen_counter' = succ gen_counter in
    let tz =
      match timekeeping with
      | `Local -> t1 +. time_per_frame
      | `Global -> t0 +. (float gen_counter') *. time_per_frame
      | `Limited_global f -> max (t1 +. time_per_frame *. f) (t0 +. (float gen_counter') *. time_per_frame)
      | _ -> assert false
    in
    let ty = Sys.time () in
    let dt = tz -. ty in
    if dt > 0.0 then begin
      nap dt;
      napped := !napped +. dt
    end;
    run gen_counter' 
  in run 0;;
  
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
  let init_shape = ref `Square in
  let air = ref 1 in
  let enlargement = ref 4 in
  let color_step = ref 8 in
  let framerate_limitator = ref None in
  let user_seed = ref None in
  let timekeeping = ref (`Limited_global 0.9) in
  let local_is_default, global_is_default, limited_global_is_default =
    match !timekeeping with
      | `Local -> " (default)", "", ""
      | `Global -> "", " (default)", ""
      | `Limited_global f -> "", "", Printf.sprintf " (default %.2f)" f
      | _ -> assert false
  in
  (try
     Arg.parse
       [("----", Arg.Unit (fun () -> ()), "--------- USEFUL OPTIONS ------------");
        ("-enl", Arg.Int (fun i -> enlargement := i), "enlargement");
        ("-step", Arg.Int (fun i -> color_step := i), "color step");
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
        ("-fps", Arg.Float (fun s -> framerate_limitator := Some s), "max framerate");
        ("-seed", Arg.Int (fun s -> user_seed := Some s), "random generator seed");
        ("----", Arg.Unit (fun () -> ()), "------ LESS USEFUL OPTIONS ----------");
        ("-circle", Arg.Unit (fun () -> init_shape := `Circle), "init area is circular");
        ("-square", Arg.Unit (fun () -> init_shape := `Square), "init area is square");
        ("-verbose", Arg.Unit (fun () -> verbose := not !verbose), "flip verbose mode");
        ("-min", Arg.Int (fun i -> min_neighbours := i), "min live neighbours for surviving");
        ("-man", Arg.Int (fun i -> max_neighbours := i), "max live neighbours for surviving");
        ("-mib", Arg.Int (fun i -> min_birth := i), "min live neighbours for birth");
        ("-mab", Arg.Int (fun i -> max_birth := i), "max live neighbours for birth");
        ("-glo", Arg.Unit (fun () -> timekeeping := `Global),
         (Printf.sprintf "global timekeeping%s" global_is_default));
        ("-loc", Arg.Unit (fun () -> timekeeping := `Local),
         (Printf.sprintf "local timekeeping%s" local_is_default));
        ("-lim", Arg.Float (fun f -> timekeeping := `Limited_global f),
         (Printf.sprintf "limited global timekeeping%s" limited_global_is_default));
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
         !radius !init_shape !init_probability
         !framerate_limitator
         !timekeeping
  in
  match !anons with
  | [y; x] -> start x y
  | [x] -> start x x
  | [] -> start 128 128
  | _ -> print_user_manual_and_die ();;
  
if not !Sys.interactive then
  main ();;
