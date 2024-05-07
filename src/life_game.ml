let xsize = 5
let ysize = 5

let now_array = Array.make_matrix (ysize+2) (xsize+2) 0

let int_to_mark n = 
  match n with
  | 0 -> Some "_"
  | 1 -> Some "*"
  | _ -> None 

let display array = 
  let rec f i j = 
    if i > ysize then ()
    else if j > xsize then ( print_newline ();
                            f (i+1) 1 )
    else ( (let str = int_to_mark array.(i).(j) in
            match str with
            | Some x -> print_string x
            | None -> ());
          f i (j+1) )
  in f 1 1 

let change array x y = 
  match array.(y).(x) with
  | 0 -> array.(y).(x) <- 1
  | 1 -> array.(y).(x) <- 0
  | _ -> ()

let count array =
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
                                  let count = array.(i).(j-1) + array.(i).(j+1) in 
                                  f i j (k+1) (count+pre)
                                  )
              else (
                    let count = array.(i+k).(j-1) + array.(i+k).(j) + array.(i+k).(j+1) in 
                    f i j (k+1) (count+pre)
                    )
              )
        ) 
  in f 1 1 (-1) 0;
  count_array

let rule now_array count_array next_array x y =
  match now_array.(y).(x) with
  | 0 -> (match count_array.(y).(x) with
          | 3 -> next_array.(y).(x) <- 1
          | _ -> ())
  | 1 -> (match count_array.(y).(x) with
          | 2 | 3 -> next_array.(y).(x) <- 1
          | _ -> next_array.(y).(x) <- 0)
  | _ -> ()


let make_next_array now_array count_array next_array =
  let rec f i j = 
  if i > ysize then print_newline ()
  else if j > xsize then (f (i+1) 1)
  else (rule now_array count_array next_array j i;
        print_string " ";
        f i (j+1))
in f 1 1;
next_array

let _ = change now_array 1 1 
let _ = change now_array 2 2
let _ = change now_array 4 3
let _ = change now_array 1 4
let _ = change now_array 2 4

let gen = 5
let loop = 
  let rec f now_array n = 
    if n < gen then g now_array n 
    else display now_array
  and g now_array n = 
      display now_array;
      let count = count now_array in 
      let next_array = make_next_array now_array count (Array.make_matrix (ysize+2) (xsize+2) 0) in 
      f next_array (n+1)
  in f now_array 0

let _ = loop 