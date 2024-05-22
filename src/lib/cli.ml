(* 文字の背景色を変更する *)
let change_background_color color_code =
  Printf.printf "\027[48;5;%dm" color_code

(* 色付きで文字を出力する *)
let print_color_message message color =
  Printf.printf "\027[38;5;%dm%s\027[0m" color message

(* 1マスを色付きで表示する *)
let print_color_cell color = 
  change_background_color color;
  print_color_message "  " color;
  change_background_color 0;;