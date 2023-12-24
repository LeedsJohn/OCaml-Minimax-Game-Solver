open Core

type tile = Blank | P1 | P2 [@@deriving compare, sexp]

(* TODO optimization: cache top piece in each column? *)
(* board: bottom left corner is (0, 0) *)
type t = { board : tile array array; turn : bool } [@@deriving sexp]
type move = int [@@deriving sexp]

let height = 6
let length = 7
let win_length = 4

(* TODO optimization: Flatten board array or use string? *)
let empty_board =
  { board = Array.make_matrix ~dimx:length ~dimy:height Blank; turn = true }

let move_of_string m : move = Int.of_string m
let in_bounds x y = 0 <= x && 0 <= y && x < length && y < height

let top_empty_row t x =
  List.find_exn (List.range 0 height) ~f:(fun y ->
      compare_tile t.board.(x).(y) Blank = 0)

let get_player_tile t = if t.turn then P1 else P2

let fast_score_move t move =
  let player_tile = get_player_tile t in
  let y = top_empty_row t move in
  List.count
    [ (move - 1, y); (move + 1, 0); (move, y - 1) ]
    ~f:(fun (x, y) ->
      in_bounds x y && compare_tile player_tile t.board.(x).(y) = 0)

let get_allowed_moves t =
  List.filter (List.range 0 length) ~f:(fun x ->
      compare_tile t.board.(x).(height - 1) Blank = 0)
  |> List.sort ~compare:(fun move1 move2 ->
         Int.compare (fast_score_move t move2) (fast_score_move t move1))

(* Assumes move is a valid column *)
let apply_move t move : t =
  let new_board = Array.copy_matrix t.board in
  let player_tile = get_player_tile t in
  let y = top_empty_row t move in
  new_board.(move).(y) <- player_tile;
  { board = new_board; turn = not t.turn }

(* If a player has n in a row, score the position (2n)^2 *)
let tile_sequence_score length = 2 * length * (2 * length) |> Int.to_float

let absolute_eval_tile board start_x start_y dx dy =
  let prev_x, prev_y = (start_x - dx, start_y - dy) in
  if
    compare_tile board.(start_x).(start_y) Blank = 0
    || in_bounds prev_x prev_y
       && compare_tile board.(start_x).(start_y) board.(prev_x).(prev_y) = 0
  then 0.
  else
    let sequence_length, (end_x, end_y) =
      List.findi_exn
        (List.init win_length ~f:(fun i ->
             (start_x + (i * dx), start_y + (i * dy))))
        ~f:(fun i (nx, ny) ->
          (not (in_bounds (nx + dx) (ny + dy)))
          || compare_tile board.(start_x).(start_y) board.(nx + dx).(ny + dy)
             <> 0
          || i + 1 = win_length)
    in
    let sequence_length = sequence_length + 1 in
    if sequence_length >= win_length then Float.infinity
    else
      let num_left =
        List.find_exn (List.range 0 win_length) ~f:(fun i ->
            let nx, ny = (start_x - ((i + 1) * dx), start_y - ((i + 1) * dy)) in
            (not (in_bounds nx ny))
            || compare_tile board.(nx).(ny) Blank <> 0
            || i + 1 = win_length)
      in
      let num_right =
        List.find_exn (List.range 0 win_length) ~f:(fun i ->
            let nx, ny = (end_x + ((i + 1) * dx), end_y + ((i + 1) * dy)) in
            (not (in_bounds nx ny))
            || compare_tile board.(nx).(ny) Blank <> 0
            || i + 1 = win_length)
      in
      if sequence_length + num_left + num_right < win_length then 0.
      else
        let num_sides_open =
          Bool.to_int (num_left > 0) + Bool.to_int (num_right > 0)
          |> Float.of_int
        in
        num_sides_open *. tile_sequence_score sequence_length

let eval_tile board x y dx dy =
  let multiplier =
    match board.(x).(y) with Blank -> 0. | P1 -> 1. | P2 -> -1.
  in
  multiplier *. absolute_eval_tile board x y dx dy

let static_eval t =
  (* check horizontal, vertical, and both diagonals *)
  let direction_checks = [ (1, 0); (0, -1); (-1, -1); (1, -1) ] in
  List.fold (List.range 0 height) ~init:0. ~f:(fun acc y ->
      List.fold (List.range 0 length) ~init:acc ~f:(fun acc x ->
          List.fold direction_checks ~init:acc ~f:(fun acc (dx, dy) ->
              acc +. eval_tile t.board x y dx dy)))

let is_over t =
  List.for_all (List.range 0 length) ~f:(fun x ->
      compare_tile t.board.(x).(height - 1) Blank <> 0)
  || Float.(abs (static_eval t) = Float.infinity)

let get_turn t = t.turn

let display t =
  List.iter
    (List.range (height - 1) (-1) ~stride:(-1))
    ~f:(fun y ->
      List.iter (List.range 0 length) ~f:(fun x ->
          let c =
            match t.board.(x).(y) with Blank -> " " | P1 -> "X" | P2 -> "O"
          in
          print_string c);
      print_endline "");
  print_endline "------------\n"
