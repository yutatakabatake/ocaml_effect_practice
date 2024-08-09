open Effect
open Effect.Deep

(* ハンドルされる関数の型 *)
type _ Effect.t += Read : unit -> string Effect.t

(* readをハンドルするハンドラを定義 *)
let handler f = 
  match_with f () {
    retc = (fun x -> x);
    exnc = (fun e -> raise e);
    effc = (fun (type b) (eff: b t) -> 
      (match eff with
        | Read () ->
          (Some (fun (k: (b,_) continuation) -> 
            let x = "Bob" in 
            continue k x)) (* 値を持って継続を再開 *)
        | _ -> None)
    )
  }

(* 実行されるプログラム *)
let printFullName () = 
  (* print_endline ("What is your forename?"); *)
  let forename = perform (Read ()) in 
    (* print_endline ("What is your surname?"); *)
    let surname = perform (Read ()) in 
      print_endline (forename ^ surname)


let _ = handler printFullName 