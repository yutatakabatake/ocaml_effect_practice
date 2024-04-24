open Effect
open Effect.Deep

type _ Effect.t += Get : unit -> int Effect.t
type _ Effect.t += Put : int -> unit Effect.t

let run f init = 
  let state = ref init in
  match_with f () {
    retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = (fun  (type b) (eff: b t)-> 
      (match eff with
        | Get () -> 
          (Some (fun (k: (b,_) continuation) -> 
            let x: int = !state in
            continue k x))
        | Put x -> 
          (Some (fun (k: (b,_) continuation) -> 
            state := x; 
            continue k ()))
        | _ -> None)
    )
  }

let f () = 
  let x = perform (Get ()) in (* エフェクトの発生 *)
  print_endline (string_of_int x);
  let _ = perform (Put 3) in  (* エフェクトの発生 *)
  let y = perform (Get ()) in 
  print_endline (string_of_int y);
  ()
  
let _ = run f 1