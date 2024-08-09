open Effect
open Effect.Deep

type _ Effect.t += Print : string -> unit Effect.t

let reverse f = 
  match_with f () {
    retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = (fun (type b) (eff: b t) -> 
      (match eff with
        | Print s ->
          (Some (fun (k: (b,_) continuation) -> 
            let _ = continue k () in (* 継続を先に再開してからprintする *)
              print_endline s))
        | _ -> None)
    )
  }


let abc () = 
  let _ = perform (Print "A") in 
  let _ = perform (Print "B") in 
  perform (Print "C")

let _ = reverse abc