let xsize = 5
let ysize = 5

let now_array = Array.make_matrix (ysize+2) (xsize+2) 0
let next_array = Array.make_matrix (ysize+2) (xsize+2) 0

let int_to_mark n = 
  match n with
  | 0 -> Some "_"
  | 1 -> Some "*"
  | _ -> None 

let display array (xsize:int) (ysize:int) = 
  for i = 1 to ysize do
    for j = 1 to xsize do
      let str = int_to_mark array.(i).(j) in
      match str with
      | Some x -> print_string x
      | None -> ()
    done;
    print_newline ();
  done;
  print_newline ()

let change array x y = 
  match array.(y).(x) with
  | 0 -> array.(y).(x) <- 1
  | 1 -> array.(y).(x) <- 0
  | _ -> ()

let count array (xsize:int) (ysize:int) =
  let count_array = Array.make_matrix (ysize+2) (xsize+2) 0 in 
  for i = 1 to ysize do
    for j = 1 to xsize do
      let count = ref 0 in
      for k = -1 to 1 do      
      count := array.(i+k).(j-1) + array.(i+k).(j) + array.(i+k).(j+1) + !count
      done;
    count := !count - array.(i).(j);
    count_array.(i).(j) <- !count
    done;
  done;
  count_array

(* let show_array array xsize ysize = 
  let rec f i j =
    print_int array.(i).(j);
    if i < ysize -1 && j < xsize -1 then f i (j+1)
    else if i < ysize -1 && j == xsize -1 then f (i+1) 0
    else if i == ysize -1 && j < xsize -1 then f i (j+1)
    else if i == ysize -1 && j == xsize -1 then ()
  in 
  f 0 0 *)

let show_array array xsize ysize = 
  for i = 1 to ysize do 
    for j = 1 to xsize do 
      print_int array.(i).(j);
    done;
    print_newline ()
  done

let rule now_array next_array x y = 
  match now_array.(y).(x) with
  | 2 -> next_array.(y).(x) <- 1
  | 3 -> next_array.(y).(x) <- 1
  | _ -> next_array.(y).(x) <- 0

let make_next_array now_array next_array xsize ysize = 
  for i = 0 to xsize -1 do 
    for j = 0 to ysize -1 do 
      rule now_array next_array j i;
    done;
  done;
  next_array


let _ = display now_array xsize ysize

let _ = show_array now_array xsize ysize

let _ = change now_array 1 1 
let _ = change now_array 2 2
let _ = change now_array 4 3
let _ = change now_array 1 4
let _ = change now_array 2 4

let _ = display now_array xsize ysize

let _ = show_array now_array xsize ysize
let _ = print_newline ()

let count = count now_array xsize ysize

let _ = show_array count xsize ysize
let _ = print_newline ()

let array2 = make_next_array count next_array xsize ysize

let _ = show_array array2 xsize ysize
let _ = print_newline ()

let _ = display array2 xsize ysize