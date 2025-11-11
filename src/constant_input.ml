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
            continue k "Bob")) (* 値を持って継続を再開 *)
        | _ -> None)
    )
  }

(* alwaysRead = fun s -> handler {read(_,k) -> k s} *)
let alwaysRead = 
  fun (s : string) -> 
    let handler2 f = 
      match_with f () {
        retc = (fun x -> x);
        exnc = (fun e -> raise e);
        effc = (fun (type b) (eff: b t) -> 
          (match eff with
            | Read () ->
              (Some (fun (k: (b,_) continuation) -> 
                continue k s)) 
            | _ -> None)
        )
      } in handler2


(* 実行されるプログラム *)
let printFullName () = 
  print_endline ("What is your forename?");
  let forename = perform (Read ()) in 
    print_endline ("What is your surname?");
    let surname = perform (Read ()) in 
      print_endline (forename ^ surname)


let _ = print_endline ("with handler handle printFullName")
let _ = handler printFullName 
(* BobBob *)

let _ = print_endline ("with (alwaysRead Bob) handle printFullName")
let _ = alwaysRead "Bob" printFullName
(* BobBob *)