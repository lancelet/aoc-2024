open Angstrom

(* ---- Parsing -------------------------------------------------------------*)

let parse_int : int t =
  take_while1 (function '0' .. '9' -> true | _ -> false)
  >>= fun s -> return (int_of_string s)

let parse_pair : (int * int) t =
  let* x = parse_int in
  let* _ = skip_many1 (char ' ') in
  let* y = parse_int in
  return (x, y)
  
let parse_pairs_list : (int * int) list t =
  let* result = sep_by end_of_line parse_pair in
  let* _ = skip_many end_of_line in
  let* _ = end_of_input in
  return result
  
let read_input (file_name : string) : (int * int) list =
  let file = In_channel.open_text file_name in
  let input = In_channel.input_all file in
  match parse_string ~consume:All parse_pairs_list input with
    | Ok pairs -> pairs
    | Error msg -> failwith ("Parsing error: " ^ msg)

(* ---- Main ----------------------------------------------------------------*)
  
let main () =
  let filename = "../inputs/input-day-01.txt" in
  let pairs = read_input filename in
  List.iter (fun (x,y) -> Printf.printf "(%d, %d)\n" x y) pairs

let () = main ()