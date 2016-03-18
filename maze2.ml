
(* compile with: ocamlopt -o maze.exe graphics.cmxa unix.cmxa maze.ml
 * 
 * http://www.conwaylife.com/w/index.php?title=Maze 
 *)

let artifex = "PETRVS·PAVLVS·NEPTVNENSIS·ME·FECIT·MMXVI";;
  
let version = "2.4";;
  
let need_to_work_around_minimize_bug = ref true;;
let need_to_work_around_float_of_string_bugs = ref true;;
let need_to_work_around_int_of_float_bug = ref true;;
  
let less_buggy_float_of_string s =
  match s with
  | "inf" -> infinity
  | "-inf" -> neg_infinity
  | "nan" -> nan
  | _ -> float_of_string s;;
  
let less_buggy_int_of_float f =
  let fallo () = failwith (Printf.sprintf "less_buggy_int_of_float: bad argument: %f" f) in
  match classify_float f with
  | FP_infinite | FP_nan -> fallo ()
  | _ -> if f < float(min_int) then fallo ()
         else if f > float(max_int) then fallo ()
         else int_of_float f;;
  
let verbose = ref false;;
  
let verbose_choose_color = ref false;;
  
open Graphics;;
  
let wrap x y =
  if x < 0 then x + y
  else if x >= y then x - y
  else x;;
  
let gen m1 m2 color neighbours_for_surviving neighbours_for_birth =
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
      let bit_k = 1 lsl !k in
      let color0 = m1.(x).(y) in
      if color0 <> white then begin
        if bit_k land neighbours_for_surviving <> 0 then begin
          m2.(x).(y) <- color0
        end else begin
          m2.(x).(y) <- white
        end;
      end else if bit_k land neighbours_for_birth <> 0 then begin
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
  let xmin = less_buggy_int_of_float (floor (max (float x0 -. radius) 0.0)) in
  let xmax = less_buggy_int_of_float (ceil (min (float xdim -. 1.0) (float x0 +. radius))) in
  let ymin = less_buggy_int_of_float (floor (max (float y0 -. radius) 0.0)) in
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
  let cell_side = magnification - air in
  let xmax = Array.length m - 1 in
  let ymax = Array.length m.(0) - 1 in
  for x = 0 to xmax do
    let col = m.(x) in
    for y = 0 to ymax do
      set_color col.(y);
      fill_rect (x * magnification) (y * magnification) cell_side cell_side
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
  
module Kahan_summation : sig
  type t;;
  val acc : unit -> t;;
  val add : t -> float -> unit;;
  val get : t -> float;;
  val reset : t -> unit;;
    
end = struct
  (* https://en.wikipedia.org/wiki/Kahan_summation_algorithm *)
  type t = { mutable sum : float;
             mutable c : float (* A running compensation for lost low-order bits. *)
           };;
    
  let acc () = { sum = 0.0;
                 c = 0.0 (* So far, so good: c is zero. *)
               };;
    
  let add ({ sum; c } as k) addend =
    let y = addend -. c in
    let t = sum +. y in	(* Alas, sum is big, y small, so low-order digits of y are lost. *)
    k.c <- (t -. sum) -. y; (* (t - sum) cancels the high-order part of y; subtracting y recovers negative (low part of y) *)
    k.sum <- t (* Algebraically, c should always be zero. Beware overly-aggressive optimizing compilers! *)
  (* Next time around, the lost low part will be added to y in a fresh attempt. *)
  ;;
    
  let get { sum } = sum;;
    
  let reset k =
    k.sum <- 0.0;
    k.c <- 0.0;;
    
end;;


module SummaryStats : sig
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
  
  module KS = Kahan_summation;;
    
  type t = {
      mutable n : float;
      mutable mean : float;
      mutable m2 : float;
      mutable sum : KS.t;
      mutable min : float;
      mutable max : float;
    };;
    
  let make () =
    { n = 0.0;
      mean = 0.0;
      m2 = 0.0;
      sum = KS.acc ();
      min = infinity;
      max = neg_infinity;
    };;
    
  let reset stat =
    stat.n <- 0.0;
    stat.mean <- 0.0;
    stat.m2 <- 0.0;
    KS.reset stat.sum;
    stat.min <- infinity;
    stat.max <- neg_infinity;;
    
  (* https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm *)
    
  let add stat x =
    (* variance *)
    stat.n <- stat.n +. 1.0;
    let delta = x -. stat.mean in
    stat.mean <- stat.mean +. delta /. stat.n;
    stat.m2 <- stat.m2 +. delta *. (x -. stat.mean);
    (* other *)
    KS.add stat.sum x;
    stat.min <- min stat.min x;
    stat.max <- max stat.max x;;
    
  let number_of_samples { n } = n;;
    
  let mean { mean } = mean;;
    
  let sample_variance { n; m2 } =  m2 /. (n -. 1.0);;
    
  let population_variance { n; m2 } = m2 /. n;;
    
  let standard_deviation stat = sqrt (population_variance stat);;
    
  let sample_standard_deviation stat = sqrt (sample_variance stat);;
    
  let sum { sum } = KS.get sum;;
    
  let get_min { min } = min;;
  let get_max { max } = max;;
    
  let range { max; min } = max -. min;;
    
  let midrange { max; min } = min +. (max -. min) /. 2.0;;
    
end;;

module SS = SummaryStats;;
  
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
         neighbours_for_surviving neighbours_for_birth
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
    | Some fps -> 1.0 /. fps, (Printf.sprintf " (target = %.2f_Hz);" fps), true
  in
  let t0 = Sys.time () in
  let stat = SS.make () in
  let next_time_inform = ref 0.0 in
  let rec run () =
    let t1 = Sys.time () in
    if !need_to_work_around_minimize_bug then begin
      auto_synchronize false;
      display_mode false;
    end;
    synchronize ();
    let gen_counter = SS.number_of_samples stat in
    if !verbose && t1 >= !next_time_inform then begin
      next_time_inform := t1 +. inform_interval;
      let dt = t1 -. t0 in
      Printf.printf "gen/time = %.0f/%.2f_s = %.2f_Hz%s\n"
                    gen_counter dt (gen_counter /. dt)
                    user_request_as_verbose_info;
      if is_fps_requested then begin
          Printf.printf "nap/total = %.2f_s/%.2f_s = %.2f%%\n"
                        (SS.sum stat)
                        dt
                        (SS.sum stat /. dt *. 100.0);
          Printf.printf "nap/gen: min = %.4f_s; mean = %.4f_s; max = %.4f_s; sd = %.4f_s;\n"
                        (SS.get_min stat)
                        (SS.mean stat)
                        (SS.get_max stat)
                        (SS.standard_deviation stat)
        end;
      flush stdout;  
    end;
    handle_keyboard ();
    display !m2 magnification air;
    gen !m1 !m2 (choose_color color color_step) neighbours_for_surviving neighbours_for_birth;
    swap m1 m2;
    let tz =
      match timekeeping with
      | `Local -> t1 +. time_per_frame
      | `Global -> t0 +.  (gen_counter +. 1.0) *. time_per_frame
      | `Limited_global f -> max (t1 +. time_per_frame *. f)
                                 (t0 +. (gen_counter +. 1.0) *. time_per_frame)
      | _ -> assert false
    in
    let ty = Sys.time () in
    let dt = tz -. ty in
    let dt' = max 0.0 dt in
    nap dt';
    SS.add stat dt';
    run ()
  in run ();;
  
let split_at_char char string =
  let len = String.length string in
  let rec fa da i acc =
    if i = len then
      List.rev (String.sub string da (i - da) :: acc)
    else if string.[i] = char then
      fa (succ i) (succ i) (String.sub string da (i - da) :: acc)
    else
      fa da (succ i) acc
  in fa 0 0 [];;
  
let string_list_to_bitmap_as_integer sl =
  List.fold_left
    (fun acc p -> acc + 1 lsl (int_of_string p))
    0
    sl;;
  
let decode_rule s =
  match split_at_char '/' s with
  | [p1; p2] ->
     let p1s = split_at_char ',' p1 in
     let p2s = split_at_char ',' p2 in
     begin try Some (string_list_to_bitmap_as_integer p1s,
                     string_list_to_bitmap_as_integer p2s)
           with Failure _ -> None
     end
  | _ -> None;;
  
let user_manual =
  Printf.sprintf "Use: %s [xdim] [ydim]\nUse: %s -help\n" Sys.argv.(0) Sys.argv.(0);;
  
let print_user_manual_and_die () =
  print_endline user_manual;
  if not !Sys.interactive then
    exit 1;;
  
let main () =
  let anons = ref [] in
  let neighbours_for_surviving = ref 0b111110 in
  let neighbours_for_birth = ref 0b1000 in
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
  let local_is_default = if !timekeeping = `Local then " (default)" else "" in
  let global_is_default = if !timekeeping = `Global then " (default)" else "" in
  let limited_global_is_default =
    match !timekeeping with
    | `Limited_global f -> Printf.sprintf " (default, value = %.2f)" f
    | _ -> ""
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
        ("-rad", Arg.String (fun s -> try radius := less_buggy_float_of_string s
                                      with Failure _ ->
                                        raise (Arg.Bad
                                                 (Printf.sprintf
                                                    "bad options: -rad %s" s))),
         (Printf.sprintf "<float>\tradius of random initial blot (default = %.2f)" !radius));
        ("-prob", Arg.Float (fun x -> init_probability := x),
         (Printf.sprintf
            "<float>\tprob of live cell in the initial blot (default = %.2f)"
            !init_probability));
        ("-life", Arg.Unit (fun () ->
                      neighbours_for_surviving := 0b1100;
                      neighbours_for_birth := 0b1000;
                      radius := 1e20), "\tsame as -rule 2,3/3 -radius inf");
        ("-maze", Arg.Unit (fun () ->
                      neighbours_for_surviving := 0b111110;
                      neighbours_for_birth := 0b1000;
                      radius := 5.0), "\tsame as -rule 1,2,3,4,5/3 -radius 5.0");
        ("-fps", Arg.Float (fun s -> framerate_limitator := Some s), "<float>\ttarget framerate");
        ("-seed", Arg.Int (fun s -> user_seed := Some s), "<int>\trandom generator seed");
        ("----", Arg.Unit (fun () -> ()), "-------\t-- LESS USEFUL OPTIONS -------");
        ("-circle", Arg.Unit (fun () -> init_shape := `Circle), "\tinit area is circular");
        ("-square", Arg.Unit (fun () -> init_shape := `Square), "\tinit area is square");
        ("-verbose", Arg.Unit (fun () -> verbose := not !verbose),
         (Printf.sprintf "\ttoggle verbose mode (default = %b)" !verbose));
        ("-rule", Arg.String (fun s -> match decode_rule s with
                                       | None -> raise (Arg.Bad "bad rule")
                                       | Some (surv, birth) ->
                                          neighbours_for_surviving := surv;
                                          neighbours_for_birth := birth),
         "<str>\tS1,...,Sn/B1,...,Bm where the S are number of neighbours to survive, B are the numbers of neighbours for birth");
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
         (Printf.sprintf "\ttoggle work around for minimize bug (default = %b)"
                         !need_to_work_around_minimize_bug));
        ("-wafos", Arg.Unit
                     (fun f ->
                       need_to_work_around_float_of_string_bugs :=
                         not !need_to_work_around_float_of_string_bugs),
         (Printf.sprintf "\ttoggle work around for bugs in float_of_string (default = %b)"
                         !need_to_work_around_float_of_string_bugs));
        ("-waiof", Arg.Unit
                     (fun f ->
                       need_to_work_around_int_of_float_bug :=
                         not !need_to_work_around_int_of_float_bug),
         (Printf.sprintf "\ttoggle work around for bugs in int_of_float (default = %b)"
                         !need_to_work_around_int_of_float_bug));
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
         !neighbours_for_surviving
         !neighbours_for_birth
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
