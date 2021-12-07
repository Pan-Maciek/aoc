open Str
open String

module Board = struct
  type location = { row: int; col: int; }

  type board = { 
    mutable score: int; 
    rows: int array; 
    cols: int array; 
    find: int -> location option
  }


  let make board = 
    let sum = List.fold_left (+) 0 board in
    let assoc = List.mapi (fun i x -> x, { row = i mod 5; col = i / 5}) board in
    { 
      score = sum; 
      rows = Array.make 5 0; 
      cols = Array.make 5 0; 
      find = (fun num -> List.assoc_opt num assoc)
    }
  
  let is_winning board = Array.exists ((==) 5) board.rows || Array.exists ((==) 5) board.cols
  
  let mark num board =
    match board.find num with
    | Some { col; row } -> 
      board.cols.(col) <- board.cols.(col) + 1;
      board.rows.(row) <- board.rows.(row) + 1;
      board.score <- board.score - num;
    | None -> ()

end
open Board

let rec take_tail k xs = 
  match k, xs with
  | 0, xs -> [], xs
  | _, [] -> failwith "take_tail"
  | k, x::xs -> 
    let (next, tail) = take_tail (k-1) xs in 
    x::next, tail

let rec chunk k xs =
  match xs with
  | [] -> []
  | xs -> let (head, tail) = take_tail k xs in
    head :: chunk k tail

let input_state ch =

  let rng_line = input_line ch in
  let boards_string = really_input_string ch (in_channel_length ch - String.length rng_line - 1) in

  let rng = rng_line 
    |> split_on_char ',' 
    |> List.map int_of_string
  in

  let boards = boards_string 
    |> Str.split (regexp "[^0-9]+")
    |> List.map int_of_string
    |> chunk 25
    |> List.map Board.make
  in

  rng, boards

let rec run_game = function
  | [], _ -> failwith "run_game"
  | r::rng, [board] ->
    Board.mark r board;
    if Board.is_winning board 
      then board.score * r
      else run_game (rng, [board])
  | r::rng, boards -> 
    List.iter (Board.mark r) boards;
    run_game (rng, List.filter (fun board -> not (Board.is_winning board)) boards)
  
let () = 

  match Sys.argv with
  | [| _; file |] ->
    let ch = open_in file in
    let game_state = input_state ch in
    game_state
    |> run_game
    |> string_of_int
    |> print_endline
  | _ -> Printf.eprintf "usage: %s <input>\n" Sys.argv.(0)