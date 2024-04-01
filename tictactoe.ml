(* run program with `ocaml tictactoe.ml` *)

type value = Nil | X | O

type board = value array

(* cell positions are "top right," "top middle," etc *)
type cell = TL | TM | TR | ML | MM | MR | BL | BM | BR

type game = Newgame | Xwin | Owin | Stalemate | Playing

let new_board =
  Array.make 9 Nil

let get_cell_value b c =
  match c with
    TL -> b.(0)
  | TM -> b.(1)
  | TR -> b.(2)
  | ML -> b.(3)
  | MM -> b.(4)
  | MR -> b.(5)
  | BL -> b.(6)
  | BM -> b.(7)
  | BR -> b.(8)

(* can't set value of a filled cell *)
let set_cell_value b c v =
  match (get_cell_value b c) with
    X | O -> Result.Error ()
    | Nil -> Result.Ok (
      match c with
        TL -> b.(0) <- v
      | TM -> b.(1) <- v
      | TR -> b.(2) <- v
      | ML -> b.(3) <- v
      | MM -> b.(4) <- v
      | MR -> b.(5) <- v
      | BL -> b.(6) <- v
      | BM -> b.(7) <- v
      | BR -> b.(8) <- v
    )

let check_game = function
    [|Nil;Nil;Nil;Nil;Nil;Nil;Nil;Nil;Nil|] -> Newgame
  | [|X;X;X;_;_;_;_;_;_|] -> Xwin
  | [|_;_;_;X;X;X;_;_;_|] -> Xwin
  | [|_;_;_;_;_;_;X;X;X|] -> Xwin
  | [|X;_;_;X;_;_;X;_;_|] -> Xwin
  | [|_;X;_;_;X;_;_;X;_|] -> Xwin
  | [|_;_;X;_;_;X;_;_;X|] -> Xwin
  | [|X;_;_;_;X;_;_;_;X|] -> Xwin
  | [|_;_;X;_;X;_;X;_;_|] -> Xwin
  | [|O;O;O;_;_;_;_;_;_|] -> Owin
  | [|_;_;_;O;O;O;_;_;_|] -> Owin
  | [|_;_;_;_;_;_;O;O;O|] -> Owin
  | [|O;_;_;O;_;_;O;_;_|] -> Owin
  | [|_;O;_;_;O;_;_;O;_|] -> Owin
  | [|_;_;O;_;_;O;_;_;O|] -> Owin
  | [|O;_;_;_;O;_;_;_;O|] -> Owin
  | [|_;_;O;_;O;_;O;_;_|] -> Owin
  | [|(X|O);(X|O);(X|O);(X|O);(X|O);(X|O);(X|O);(X|O);(X|O)|] -> Stalemate
  | _ -> Playing

(* return list of board rows/cols *)
type rows =
  Trow | Mrow | Brow | Lcol | Mcol | Rcol | Lrdiag | Rldiag

let explode_board b = [
    Trow , List.map (get_cell_value b) [TL; TM; TR] ;
    Mrow , List.map (get_cell_value b) [ML; MM; MR] ;
    Brow , List.map (get_cell_value b) [BL; BM; BR] ;
    Lcol , List.map (get_cell_value b) [TL; ML; BL] ;
    Mcol , List.map (get_cell_value b) [TM; MM; BM] ;
    Rcol , List.map (get_cell_value b) [TR; MR; BR] ;
    Lrdiag , List.map (get_cell_value b) [TL; MM; BR] ;
    Rldiag , List.map (get_cell_value b) [TR; MM; BL]
  ]

(* printing the board *)

let string_of_value = function
    Nil -> " "
  | X -> "X"
  | O -> "O"

let print_board b =
  let s = Array.map (string_of_value) b
  in
  let row1 = s.(0) ^ "|" ^ s.(1) ^ "|" ^ s.(2)
  in
  let row2 = "-----"
  in
  let row3 = s.(3) ^ "|" ^ s.(4) ^ "|" ^ s.(5)
  in
  let row4 = "-----"
  in
  let row5 = s.(6) ^ "|" ^ s.(7) ^ "|" ^ s.(8)
  in
  print_newline ();
  List.iter (print_endline) [row1;row2;row3;row4;row5];
  print_newline ()

(* playing the game *)
type player = Xhuman | Xcpueasy | Xcpuhard | Ohuman | Ocpueasy | Ocpuhard

let cell_of_string s =
  match (String.uppercase_ascii s) with
    "TL" -> Result.Ok TL
  | "TM" -> Result.Ok TM
  | "TR" -> Result.Ok TR
  | "ML" -> Result.Ok ML
  | "MM" -> Result.Ok MM
  | "MR" -> Result.Ok MR
  | "BL" -> Result.Ok BL
  | "BM" -> Result.Ok BM
  | "BR" -> Result.Ok BR
  | _ -> Result.Error ()

(* dumb cpu - randomly choose an empty cell *)
let get_random_cell b =
  let f c =
    match (get_cell_value b c) with
      Nil -> true
    | X | O -> false
  in
  let candidates =
    List.filter f [TL;TM;TR;ML;MM;MR;BL;BM;BR]
  in
  Random.self_init() ;
  List.nth candidates (Random.int (List.length candidates))

(* smart cpu *)

(* rule 1: is there a winning cell? *)
let find_winner b p =
  let f x =
    match p, (fst x), (snd x) with
    | Xcpuhard, rname, [X;X;Nil] -> Some (rname,2)
    | Xcpuhard, rname, [X;Nil;X] -> Some (rname,1)
    | Xcpuhard, rname, [Nil;X;X] -> Some (rname,0)
    | Ocpuhard, rname, [O;O;Nil] -> Some (rname,2)
    | Ocpuhard, rname, [O;Nil;O] -> Some (rname,1)
    | Ocpuhard, rname, [Nil;O;O] -> Some (rname,0)
    | _ -> None
  in
  match (List.find_map f (explode_board b)) with
    Some (Trow,0) -> Some TL
  | Some (Trow,1) -> Some TM
  | Some (Trow,2) -> Some TR
  | Some (Mrow,0) -> Some ML
  | Some (Mrow,2) -> Some MR
  | Some (Brow,0) -> Some BL
  | Some (Brow,1) -> Some BM
  | Some (Brow,2) -> Some BR
  | Some (Lcol,0) -> Some TL
  | Some (Lcol,1) -> Some ML
  | Some (Lcol,2) -> Some BL
  | Some (Mcol,0) -> Some TM
  | Some (Mcol,2) -> Some BM
  | Some (Rcol,0) -> Some TR
  | Some (Rcol,1) -> Some MR
  | Some (Rcol,2) -> Some BR
  | Some (Lrdiag,0) -> Some TL
  | Some (Lrdiag,2) -> Some BR
  | Some (Rldiag,0) -> Some TR
  | Some (Rldiag,2) -> Some BL
  | Some _ -> None
  | None -> None

(* rule 2: is there a cell that must be blocked? *)
let find_threat b p =
  let f x =
    match p, (fst x), (snd x) with
    | Ocpuhard , rname, [X;X;Nil] -> Some (rname,2)
    | Ocpuhard , rname, [X;Nil;X] -> Some (rname,1)
    | Ocpuhard , rname, [Nil;X;X] -> Some (rname,0)
    | Xcpuhard , rname, [O;O;Nil] -> Some (rname,2)
    | Xcpuhard , rname, [O;Nil;O] -> Some (rname,1)
    | Xcpuhard , rname, [Nil;O;O] -> Some (rname,0)
    | _ -> None
  in
  match (List.find_map f (explode_board b)) with
    Some (Trow,0) -> Some TL
  | Some (Trow,1) -> Some TM
  | Some (Trow,2) -> Some TR
  | Some (Mrow,0) -> Some ML
  | Some (Mrow,2) -> Some MR
  | Some (Brow,0) -> Some BL
  | Some (Brow,1) -> Some BM
  | Some (Brow,2) -> Some BR
  | Some (Lcol,0) -> Some TL
  | Some (Lcol,1) -> Some ML
  | Some (Lcol,2) -> Some BL
  | Some (Mcol,0) -> Some TM
  | Some (Mcol,2) -> Some BM
  | Some (Rcol,0) -> Some TR
  | Some (Rcol,1) -> Some MR
  | Some (Rcol,2) -> Some BR
  | Some (Lrdiag,0) -> Some TL
  | Some (Lrdiag,2) -> Some BR
  | Some (Rldiag,0) -> Some TR
  | Some (Rldiag,2) -> Some BL
  | Some _ -> None
  | None -> None

(* rule 8: mark empty side -- out of order b/c we use this fn to block
   double fork *)
let empty_side b =
  (* get empty sides and pick one at random *)

  let candidates = List.filter (
                       fun x -> (get_cell_value b x) == Nil) [TM; ML; MR; BM]
  in
  if (List.length candidates) == 0 then
    None
  else (
    Random.self_init() ;
    Some (List.nth candidates (Random.int (List.length candidates)))
  )
    
(* rule 3: can player create a fork? *)
(* rule 4: block fork *)
(* fork logic via https://p-mckenzie.github.io/2020/07/30/tic-tac-toe/
   My reading is that there's no need to have a separate function to
   create a fork; that blocking forks is sufficient for perfect play.
   I'm not certain, however.

   Relevant comment from source: "Note that as the computer (player O)
   starts second, it has been programmed to play a defensive game,
   only blocking forks or 2-in-a-row rather than attempting to
   fork."
 *)

(* if double fork found, mark an empty side to defend *)
let find_double_fork b p =
  let exploded = explode_board b
  in
  if p==Xcpuhard && (List.assoc Lrdiag exploded)==[O;X;O] then
    empty_side b
  else if p==Xcpuhard && (List.assoc Rldiag exploded)==[O;X;O] then
    empty_side b
  else if p==Ocpuhard && (List.assoc Lrdiag exploded)==[X;O;X] then
    empty_side b
  else if p==Ocpuhard && (List.assoc Rldiag exploded)==[X;O;X] then
    empty_side b
  else
    None

(* if single fork found, block *)
let find_single_fork b p =
  let gcv = get_cell_value b
  in
  if p==Xcpuhard && (gcv TL)==Nil && (gcv TM)==O && (gcv ML)==O then
    Some TL
  else if p=Xcpuhard && (gcv BL)==Nil && (gcv BM)==O && (gcv ML)==O then
    Some BL
  else if p=Xcpuhard && (gcv TR)==Nil && (gcv TM)==O && (gcv MR)==O then
    Some TR
  else if p=Xcpuhard && (gcv BR)==Nil && (gcv BM)==O && (gcv MR)==O then
    Some BR
  else if p==Ocpuhard && (gcv TL)==Nil && (gcv TM)==X && (gcv ML)==X then
    Some TL
  else if p=Ocpuhard && (gcv BL)==Nil && (gcv BM)==X && (gcv ML)==X then
    Some BL
  else if p=Ocpuhard && (gcv TR)==Nil && (gcv TM)==X && (gcv MR)==X then
    Some TR
  else if p=Ocpuhard && (gcv BR)==Nil && (gcv BM)==X && (gcv MR)==X then
    Some BR
  else
    None

           (*  patterns of single forks
             X, TL _, TM O, ML O
             X, BL _, BM O, ML O
             X, TR _, TM O, MR O
             X, BR _, BM O, MR O

             O, TL _, TM X, ML X
             O, BL _, BM X, ML X
             O, TR _, TM X, MR X
             O, BR _, BM X, MR X
            *)
  
(* rule 5: mark center *)
let empty_center b =
  if Nil==get_cell_value b MM then
    Some MM
  else
    None

(* rule 6: if opponent in corner, mark opposite corner *)
let find_opposite_corner b p =
  let exploded = explode_board b
  in
  let f x =
    match p,x with
    | Xcpuhard , (Lrdiag, [O;_;Nil]) -> Some BR
    | Xcpuhard , (Lrdiag, [Nil;_;O]) -> Some TL
    | Xcpuhard , (Rldiag, [Nil;_;O]) -> Some TR
    | Xcpuhard , (Rldiag, [O;_;Nil]) -> Some BL
    | Ocpuhard , (Lrdiag, [X;_;Nil]) -> Some BR
    | Ocpuhard , (Lrdiag, [Nil;_;X]) -> Some TL
    | Ocpuhard , (Rldiag, [Nil;_;X]) -> Some TR
    | Ocpuhard , (Rldiag, [X;_;Nil]) -> Some BL
    | _ -> None
  in
  List.find_map f exploded

(* rule 7: mark empty corner *)
let empty_corner b =
  (* get empty corners and pick one at random *)
  let candidates = List.filter (
                       fun x -> (get_cell_value b x) == Nil) [TL; TR; BL; BR]
  in
  if (List.length candidates) == 0 then
    None
  else (
    Random.self_init() ;
    Some (List.nth candidates (Random.int (List.length candidates)))
  )
                       
(* find optimal cell by generating a list with each cells for each
   rule, in order of precedence.  grab the first valid output. *)
let get_optimal_cell b p =
  let l = [
    find_winner b p ;
    find_threat b p ;
    find_double_fork b p ;
    find_single_fork b p ;
    empty_center b ;
    find_opposite_corner b p ;
    empty_corner b ;
    empty_side b
    ]
  in
  List.find_map Fun.id l

(* prompt player to play their turn *)
let player_move b player =
  let prompt = match player with
      Xhuman | Xcpueasy | Xcpuhard  -> "X's turn: "
      | Ohuman | Ocpueasy | Ocpuhard -> "O's turn: "
  in
  print_string prompt ;
  let move = match player with
      Xcpueasy | Ocpueasy -> get_random_cell b
      | Xcpuhard | Ocpuhard -> (
        match (get_optimal_cell b player) with
          Some x -> x
        | None -> failwith "can't happen"
      )
      | Xhuman | Ohuman -> (
        match (cell_of_string (read_line())) with
                Result.Ok x -> x
              | Result.Error () -> failwith "Invalid cell; try again"
      )
  in
  let move_result = match player with
    Xhuman | Xcpueasy | Xcpuhard -> (set_cell_value b move X)
  | Ohuman | Ocpueasy | Ocpuhard -> (set_cell_value b move O)
  in
  match (move_result) with
    Result.Ok () -> ()
  | Result.Error () -> failwith "Cell filled; choose another"

(* count the number of xs and os already played *)
let count_xs_and_os b =
  let f (sumx, sumo) c =
    match c with
      X -> (sumx + 1, sumo)
    | O -> (sumx, sumo + 1)
    | _ -> (sumx, sumo)
  in
  Array.fold_left f (0,0) b

let whos_turn b xplayer oplayer =
  (* figure out who's turn it is; this is done by counting the number
     of Xs and Os.  if N are equal, it's X's turn*)
  let ns = count_xs_and_os b
  in
  if (fst ns) == (snd ns) then
    xplayer
  else
    oplayer

(* check board for a winner/stalemate or play next turn  *)
let rec game_loop b xplayer oplayer =
  try
    match (check_game b) with
      Stalemate -> print_endline "How about a nice game of chess?"
    | Xwin -> print_endline "X wins!"
    | Owin -> print_endline "O wins!"
    | _ -> (player_move b (whos_turn b xplayer oplayer);
            print_board b;
            game_loop b xplayer oplayer)
  with
    Failure s -> (
    print_newline () ;
    print_endline s ;
    game_loop b xplayer oplayer
  )

(* init game *)
let new_game =
    let board = new_board
    in

    let s0 = "Jude's Tic-Tac-Toe\n\n"
    in
    print_string s0 ;
    
    let s1 = "Chose player X:\n"
             ^ "1. Human (default)\n"
             ^ "2. CPU (easy)\n"
             ^ "3. CPU (hard)\n"
    in
    print_string s1 ;

    let xplayer = match (read_line()) with
        "2" -> Xcpueasy
      | "3" -> Xcpuhard
      | "" | _ -> Xhuman
    in

    let s2 = "Chose player O:\n"
             ^ "1. Human (default)\n"
             ^ "2. CPU (easy)\n"
             ^ "3. CPU (hard)\n"
    in
    print_string s2 ;

    let oplayer = match (read_line()) with
        "2" -> Ocpueasy
      | "3" -> Ocpuhard
      | "" | _ -> Ohuman
    in
    print_board board ;
    game_loop board xplayer oplayer
