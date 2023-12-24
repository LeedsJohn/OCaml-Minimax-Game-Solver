open! Core

(* Tic Tac Toe game satisfying game interface. X always moves first. *)

type t = string [@@deriving sexp]
type move = int * int * char [@@deriving sexp]

let empty_board = "         "
let board_of_string s = s
let coord_to_i x y = x + (3 * y)
let get_cell t x y = String.get t (coord_to_i x y)

let get_rows t =
  List.map (List.range 0 3) ~f:(fun y ->
      String.of_list [ get_cell t 0 y; get_cell t 1 y; get_cell t 2 y ])

let get_cols t =
  List.map (List.range 0 3) ~f:(fun x ->
      String.of_list [ get_cell t x 0; get_cell t x 1; get_cell t x 2 ])

let get_diagonals t =
  [
    String.of_list [ get_cell t 0 0; get_cell t 1 1; get_cell t 2 2 ];
    String.of_list [ get_cell t 2 0; get_cell t 1 1; get_cell t 0 2 ];
  ]

let get_allowed_moves t =
  let play_token =
    if String.count t ~f:(Char.equal ' ') % 2 = 1 then 'X' else 'O'
  in
  List.fold (List.range 0 3) ~init:[] ~f:(fun acc x ->
      List.fold (List.range 0 3) ~init:acc ~f:(fun acc y ->
          if Char.(get_cell t x y = ' ') then (x, y, play_token) :: acc else acc))

let apply_move t (x, y, c) =
  String.mapi t ~f:(fun i old -> if i = coord_to_i x y then c else old)

let static_eval t =
  let all_lines = get_rows t @ get_cols t @ get_diagonals t in
  if
    List.exists all_lines ~f:(fun line ->
        String.for_all line ~f:(Char.equal 'X'))
  then Float.infinity
  else if
    List.exists all_lines ~f:(fun line ->
        String.for_all line ~f:(Char.equal 'O'))
  then Float.neg_infinity
  else 0.

let is_over t =
  String.for_all t ~f:(fun c -> Char.(c <> ' '))
  || Float.(abs (static_eval t) > 0.01)

let get_turn t = String.count t ~f:(Char.equal ' ') % 2 = 1

let display t =
  print_endline "-----";
  for y = 0 to 2 do
    printf "|%c%c%c|\n" (get_cell t 0 y) (get_cell t 1 y) (get_cell t 2 y)
  done;
  print_endline "-----"

let%expect_test "game" =
  let f = Fn.flip apply_move in
  let t =
    empty_board
    |> f (1, 0, 'X')
    |> f (0, 0, 'O')
    |> f (1, 1, 'X')
    |> f (2, 0, 'O')
    |> f (1, 2, 'X')
  in
  display t;
  [%expect {|
    -----
    |OXO|
    | X |
    | X |
    ----- |}];
  printf "Eval: %f" (static_eval t);
  [%expect {| Eval: inf |}];
  let t = board_of_string "X   X   X" in
  display t;
  [%expect {|
    -----
    |X  |
    | X |
    |  X|
    ----- |}];
  printf "Eval: %f" (static_eval t);
  [%expect {| Eval: inf |}];
  let t = board_of_string "  O O O  " in
  display t;
  [%expect {|
    -----
    |  O|
    | O |
    |O  |
    ----- |}];
  printf "Eval: %f" (static_eval t);
  [%expect {| Eval: -inf |}];
  let t = board_of_string " X  X  X " in
  display t;
  [%expect {|
    -----
    | X |
    | X |
    | X |
    ----- |}];
  printf "Eval: %f" (static_eval t);
  [%expect {| Eval: inf |}];
  let t = board_of_string "      OOO" in
  display t;
  [%expect {|
    -----
    |   |
    |   |
    |OOO|
    ----- |}];
  printf "Eval: %f" (static_eval t);
  [%expect {| Eval: -inf |}];
  ()
