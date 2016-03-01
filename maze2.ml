
(* compile with: ocamlopt -o maze.exe graphics.cmxa unix.cmxa maze.ml
 * 
 * http://www.conwaylife.com/w/index.php?title=Maze 
 *)

let artifex = "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI";;
  
let version = "2.4";;
  
let need_to_work_around_minimize_bug = ref true;;
  
let verbose = ref false;;
  
let verbose_choose_color = ref false;;
  
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
  let radius2 = radius *. radius in
  let xmin = int_of_float (floor (max (float x0 -. radius) 0.0)) in
  let xmax = int_of_float (ceil (min (float xdim -. 1.0) (float x0 +. radius))) in
  let ymin = int_of_float (floor (max (float y0 -. radius) 0.0)) in
  let ymax = int_of_float (ceil (min (float ydim -. 1.0) (float y0 +. radius))) in
  for x = xmin to xmax do
    for y = ymin to ymax do
      let dx = float (x - x0) in
      let dy = float (y - y0) in
      if shape = `Square || shape = `Circle && dx *. dx +. dy *. dy <= radius2 then
        if Random.float 1.0 < init_probability then
          m.(x).(y) <- black
    done
  done;;
  
let display m magnification air =
  let magnification' = magnification - air in
  let xmax = Array.length m - 1 in
  let ymax = Array.length m.(0) - 1 in
  for x = 0 to xmax do
    let col = m.(x) in
    for y = 0 to ymax do
      set_color col.(y);
      fill_rect (x * magnification) (y * magnification) magnification' magnification'
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
  if !verbose_choose_color then
    Printf.printf "choose_color: chosen = 0x%.6x (%d, %d, %d)\n"
                  color k.(0) k.(1) k.(2);
  color;;
  
let nap s =
  if s > 0.0 then
    ignore (Unix.select [] [] [] s);;
  
let handle_keyboard () =
  if key_pressed () then begin
    while key_pressed () do
      ignore (read_key ())
    done;
    exit 0
  end;;
  
module Stat : sig
  type t;;
  val make : unit -> t;;
  val reset : t -> unit;;
  val add : t -> float -> unit;;
  val number_of_samples : t -> float;;
  val mean : t -> float;;
  val sample_variance : t -> float;;
  val population_variance : t -> float;;
  val standard_deviation : t -> float;;
  val sample_standard_deviation : t -> float;;
  val sum : t -> float;;
  val get_min : t -> float;;
  val get_max : t -> float;;
  val range : t -> float;;
  val midrange : t -> float;;

end = struct
  
  type t = {
      mutable k : float;
      mutable a : float;
      mutable q : float;
      mutable sum : float;
      mutable min : float;
      mutable max : float;
    };;
    
  let make () =
    { k = 0.0;
      a = 0.0;
      q = 0.0;
      sum = 0.0;
      min = infinity;
      max = neg_infinity;
    };;
    
  let reset stat =
    stat.k <- 0.0;
    stat.a <- 0.0;
    stat.q <- 0.0;
    stat.sum <- 0.0;
    stat.min <- infinity;
    stat.max <- neg_infinity;;
    
  (* https://en.wikipedia.org/wiki/Standard_deviation#Rapid_calculation_methods *)
    
  let add stat x = 
    stat.k <- stat.k +. 1.0;
    let a' = stat.a in
    stat.a <- a' +. (x -. a') /. stat.k;
    stat.q <- stat.q +. (x -. a') *. (x -. stat.a);
    stat.sum <- stat.sum +. x;
    stat.min <- min stat.min x;
    stat.max <- max stat.max x;;
    
  let number_of_samples { k } = k;;
    
  let mean { a } = a;;
    
  let sample_variance { k; q } =  q /. (k -. 1.0);;
    
  let population_variance { k; q } = q /. k;;
    
  let standard_deviation stat = sqrt (population_variance stat);;
    
  let sample_standard_deviation stat = sqrt (sample_variance stat);;
    
  let sum { sum } = sum;;
    
  let get_min { min } = min;;
  let get_max { max } = max;;
    
  let range { max; min } = max -. min;;
    
  let midrange { max; min } = min +. (max -. min) /. 2.0;;
    
end;;
  
let swap ref1 ref2 =
  let temp = !ref1 in
  ref1 := !ref2;
  ref2 := temp;;
  
let better_open_graph xdim ydim =
  (* Workaround for Graphics incompatibilities between different OSes. *)
  let xcrud, ycrud = 
    match Sys.os_type with
    | "Win32" -> 20, 50 (* Observational *)
    | _ -> 0, 0 in
  open_graph (Printf.sprintf " %dx%d" (xdim + xcrud) (ydim + ycrud))
  (* /Workaround *)
  ;;
    
let maze xdim ydim
         min_neighbours max_neighbours
         min_birth max_birth
         magnification air
         color_step
         radius shape init_probability
         framerate_limitator
         timekeeping
         inform_interval =
  better_open_graph (xdim * magnification) (ydim * magnification);
  set_window_title (Printf.sprintf "Maze %s" version);
  (* does not work on Windows *)
  (* resize_window (xdim * magnification) (ydim * magnification); *)
  auto_synchronize false;
  display_mode false;
  let m1 = ref (Array.make_matrix xdim ydim white) in
  let m2 = ref (Array.make_matrix xdim ydim white) in
  init !m1 radius shape init_probability;
  let color = Array.make 3 0 in
  let time_per_frame, user_request_as_verbose_info, is_fps_requested =
    match framerate_limitator with
    | None -> 0.0, "", false
    | Some fps -> 1.0 /. fps, (Printf.sprintf " (%.2f_Hz requested)" fps), true
  in
  let t0 = Sys.time () in
  let stat = Stat.make () in
  let next_time_inform = ref 0.0 in
  let rec run gen_counter =
    let t1 = Sys.time () in
    if !need_to_work_around_minimize_bug then begin
      auto_synchronize false;
      display_mode false;
    end;
    synchronize ();
    if !verbose && t1 >= !next_time_inform then begin
      next_time_inform := t1 +. inform_interval;
      let gen_counter_float = float gen_counter in
      let dt = t1 -. t0 in
      Printf.printf "gen/time = %d/%.2f_s = %.2f_Hz%s\n"
                    gen_counter dt (gen_counter_float /. dt)
                    user_request_as_verbose_info;
      if is_fps_requested then begin
          Printf.printf "nap/total = %.2f_s/%.2f_s = %.2f%%\n"
                        (Stat.sum stat)
                        dt
                        (Stat.sum stat /. dt *. 100.0);
          Printf.printf "nap/gen: min = %.4f_s; mean = %.4f_s; max = %.4f_s; sd = %.4f_s;\n"
                        (Stat.get_min stat)
                        (Stat.mean stat)
                        (Stat.get_max stat)
                        (Stat.standard_deviation stat)
        end;
      flush stdout;  
    end;
    handle_keyboard ();
    display !m2 magnification air;
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
    let dt' = max 0.0 dt in
    nap dt';
    Stat.add stat dt';
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
  let radius = ref 5.0 in
  let init_probability = ref 0.5 in
  let init_shape = ref `Square in
  let air = ref 1 in
  let magnification = ref 4 in
  let color_step = ref 8 in
  let framerate_limitator = ref None in
  let user_seed = ref None in
  let timekeeping = ref (`Limited_global 0.7) in
  let inform_interval = ref 1.0 in
  let local_is_default, global_is_default, limited_global_is_default =
    match !timekeeping with
      | `Local -> " (default)", "", ""
      | `Global -> "", " (default)", ""
      | `Limited_global f -> "", "", Printf.sprintf " (default, value = %.2f)" f
      | _ -> assert false
  in
  (try
     Arg.parse
       [("----", Arg.Unit (fun () -> ()), "-------\t-- USEFUL OPTIONS ------------");
        ("-mag", Arg.Int (fun i -> magnification := i),
         (Printf.sprintf "<int>\tmagnification, pixels x cell (default = %d)" !magnification));
        ("-air", Arg.Int (fun i -> air := i),
         (Printf.sprintf "<int>\tempty pixels between cells (default = %d)" !air));
        ("-step", Arg.Int (fun i -> color_step := i),
         (Printf.sprintf "<int>\tcolor step (default = %d)" !color_step));
        ("-rad", Arg.Float (fun r -> radius := r),
         (Printf.sprintf "<float>\tradius of random initial blot (default = %.2f)" !radius));
        ("-prob", Arg.Float (fun x -> init_probability := x),
         (Printf.sprintf
            "<float>\tprob of live cell in the initial blot (default = %.2f)"
            !init_probability));
        ("-life", Arg.Unit (fun () ->
                      min_neighbours := 2;
                      max_neighbours := 3;
                      min_birth := 3;
                      max_birth := 3;
                      radius := 1e20), "\tset default parameters for life");
        ("-maze", Arg.Unit (fun () ->
                      min_neighbours := 1;
                      max_neighbours := 5;
                      min_birth := 3;
                      max_birth := 3;
                      radius := 5.0), "\tset default parameters for maze");
        ("-fps", Arg.Float (fun s -> framerate_limitator := Some s), "<float>\ttarget framerate");
        ("-seed", Arg.Int (fun s -> user_seed := Some s), "<int>\trandom generator seed");
        ("----", Arg.Unit (fun () -> ()), "-------\t-- LESS USEFUL OPTIONS -------");
        ("-circle", Arg.Unit (fun () -> init_shape := `Circle), "\tinit area is circular");
        ("-square", Arg.Unit (fun () -> init_shape := `Square), "\tinit area is square");
        ("-verbose", Arg.Unit (fun () -> verbose := not !verbose),
         (Printf.sprintf "\ttoggle verbose mode (default = %b)" !verbose));
        ("-min", Arg.Int (fun i -> min_neighbours := i), "<int>\tmin live neighbours for surviving");
        ("-man", Arg.Int (fun i -> max_neighbours := i), "<int>\tmax live neighbours for surviving");
        ("-mib", Arg.Int (fun i -> min_birth := i), "<int>\tmin live neighbours for birth");
        ("-mab", Arg.Int (fun i -> max_birth := i), "<int>\tmax live neighbours for birth");
        ("-glob", Arg.Unit (fun () -> timekeeping := `Global),
         (Printf.sprintf "\tglobal timekeeping%s" global_is_default));
        ("-locl", Arg.Unit (fun () -> timekeeping := `Local),
         (Printf.sprintf "\tlocal timekeeping%s" local_is_default));
        ("-glim", Arg.Float (fun f -> timekeeping := `Limited_global f),
         (Printf.sprintf "<float>\tlimited global timekeeping%s" limited_global_is_default));
        ("-iint", Arg.Float (fun f -> inform_interval := f),
         (Printf.sprintf "<float>\tverbose information interval (default = %.2f_s)"
                         !inform_interval));
        ("-wamb", Arg.Unit
                    (fun f ->
                      need_to_work_around_minimize_bug := not !need_to_work_around_minimize_bug),
         (Printf.sprintf "\ttoggle work around minimize bug (default = %b)"
                         !need_to_work_around_minimize_bug));
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
    Printf.printf "-seed %d\n%!" seed;
    maze xdim ydim
         !min_neighbours !max_neighbours
         !min_birth !max_birth
         !magnification !air
         !color_step
         !radius !init_shape !init_probability
         !framerate_limitator
         !timekeeping
         !inform_interval
  in
  match !anons with
  | [y; x] -> start x y
  | [x] -> start x x
  | [] -> start 128 128
  | _ -> print_user_manual_and_die ();;
  
if not !Sys.interactive then
  main ();;
