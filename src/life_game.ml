open Effect
open Effect.Deep

type _ Effect.t += Display : unit -> unit Effect.t
type _ Effect.t += Count : unit -> int array array Effect.t
type _ Effect.t += Make_next_array : int array array -> unit Effect.t
type _ Effect.t += Change : (int * int) -> unit Effect.t
type _ Effect.t += Step : unit -> unit Effect.t

let xsize = 5
let ysize = 5
let gen = 5

let int_to_mark n = 
  match n with
  | 0 -> Some "_"
  | 1 -> Some "*"
  | _ -> None 

let rule now_array count_array next_array x y =
  match now_array.(y).(x) with
  | 0 -> (match count_array.(y).(x) with
          | 3 -> next_array.(y).(x) <- 1
          | _ -> ())
  | 1 -> (match count_array.(y).(x) with
          | 2 | 3 -> next_array.(y).(x) <- 1
          | _ -> next_array.(y).(x) <- 0)
  | _ -> ()

let run f = 
  let now_array = Array.make_matrix (ysize+2) (xsize+2) 0 in 
  let next_array = Array.make_matrix (ysize+2) (xsize+2) 0 in
  match_with f () {
    retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = (fun  (type b) (eff: b t)-> 
      (match eff with
        | Display () -> 
          (Some (fun (k: (b,_) continuation) ->
            let rec f i j = 
              if i > ysize then ()
              else if j > xsize then (print_newline ();
                                      f (i+1) 1)
              else ((let str = int_to_mark now_array.(i).(j) in
                      match str with
                      | Some x -> print_string x
                      | None -> ());
                    f i (j+1))
            in f 1 1;
            continue k ()
          ))
        | Count () ->
          (Some (fun (k: (b,_) continuation) ->
            let count_array = Array.make_matrix (ysize+2) (xsize+2) 0 in
            let rec f i j k pre = 
            if i > ysize then ()
            else (
                  if j > xsize then f (i+1) 1 (-1) 0 
                  else (
                        if k > 1 then (
                                      count_array.(i).(j) <- pre;
                                      f i (j+1) (-1) 0
                                      )
                        else if k == 0 then(
                                            let count = now_array.(i).(j-1) + now_array.(i).(j+1) in 
                                            f i j (k+1) (count+pre)
                                            )
                        else (
                              let count = now_array.(i+k).(j-1) + now_array.(i+k).(j) + now_array.(i+k).(j+1) in 
                              f i j (k+1) (count+pre)
                              )
                        )
                  ) 
            in f 1 1 (-1) 0;
            continue k count_array
          ))
        | Make_next_array (count_array) ->
          (Some (fun (k: (b,_) continuation) ->
            let rec f i j = 
              if i > ysize then ()
              else if j > xsize then (f (i+1) 1)
              else (rule now_array count_array next_array j i;
                    f i (j+1))
            in f 1 1;
            continue k ()
          ))
        | Change (x,y) ->
          (Some (fun (k: (b,_) continuation) ->
            (match now_array.(y).(x) with
            | 0 -> now_array.(y).(x) <- 1
            | 1 -> now_array.(y).(x) <- 0
            | _ -> ()
            );
            continue k ()
          ))
        | Step () ->
          (Some (fun (k: (b,_) continuation) ->
            let rec f i j = 
              if i > ysize then ()
              else if j > xsize then (f (i+1) 1)
              else (now_array.(i).(j) <- next_array.(i).(j) ;
                    f i (j+1))
            in f 1 1;
            continue k ()
          ))
        | _ -> None)
    )
  }

let main () =
  let _ = perform (Change (1,1)) in 
  let _ = perform (Change (2,2)) in 
  let _ = perform (Change (4,3)) in 
  let _ = perform (Change (1,4)) in 
  let _ = perform (Change (2,4)) in 
  let rec loop n = 
    if n < gen then (let _ = perform (Display ()) in 
                     let count = perform (Count ()) in 
                     let _ = perform (Make_next_array count) in 
                     let _ = perform (Step ()) in 
                     print_newline ();
                     (loop (n+1))
                    )
    else perform (Display ()) 
  in loop 0

let _ = run main 