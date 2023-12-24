open! Core

type color = Black | White [@@deriving compare, equal, sexp]

let flip_color = function Black -> White | White -> Black

type pawn = { color : color; en_passantable : bool; has_moved : bool }
[@@deriving compare, equal, sexp]

type knight = { color : color } [@@deriving compare, equal, sexp]
type bishop = { color : color } [@@deriving compare, equal, sexp]

type rook = { color : color; has_moved : bool }
[@@deriving compare, equal, sexp]

type queen = { color : color } [@@deriving compare, equal, sexp]

type king = { color : color; has_moved : bool }
[@@deriving compare, equal, sexp]

type tile =
  | Empty
  | Pawn of pawn
  | Knight of knight
  | Bishop of bishop
  | Rook of rook
  | Queen of queen
  | King of king
[@@deriving compare, equal, sexp]

type castle = Queenside | Kingside
type promo_type = To_knight | To_bishop | To_rook | To_queen

type move =
  | Move of ((int * int) * (int * int))
  | Attack of ((int * int) * (int * int))
  | Move_or_attack of ((int * int) * (int * int))
  | Castle of castle
  | En_passant of ((int * int) * (int * int) * (int * int))
    (* (from, to, destroyed pawn) *)
  | Promotion of (promo_type * (int * int) * (int * int))

module Board = struct
  type t = { board : tile array array; turn : color }

  let start_position =
    let board = Array.make_matrix ~dimx:8 ~dimy:8 Empty in
    let make_pawn color =
      Pawn { color; en_passantable = false; has_moved = false }
    in
    let make_knight color = Knight { color } in
    let make_bishop color = Bishop { color } in
    let make_rook color = Rook { color; has_moved = false } in
    let make_queen color = Queen { color } in
    let make_king color = King { color; has_moved = false } in
    let piece_positions =
      [
        (`Pawn, List.range 0 8);
        (`Rook, [ 0; 7 ]);
        (`Knight, [ 1; 6 ]);
        (`Bishop, [ 2; 5 ]);
        (`Queen, [ 3 ]);
        (`King, [ 4 ]);
      ]
    in
    List.iter piece_positions ~f:(fun (piece_type, xs) ->
        let get_piece color =
          match piece_type with
          | `Pawn -> make_pawn color
          | `Knight -> make_knight color
          | `Bishop -> make_bishop color
          | `Rook -> make_rook color
          | `Queen -> make_queen color
          | `King -> make_king color
        in
        List.iter xs ~f:(fun x ->
            board.(x).(0) <- get_piece White;
            board.(x).(7) <- get_piece Black));
    { board; turn = White }

  let all_squares = List.cartesian_product (List.range 0 8) (List.range 0 8)
  let in_bounds x y = x >= 0 && y >= 0 && x < 8 && y < 8

  let get_tile_color t x y =
    match t.board.(x).(y) with
    | Empty -> None
    | Pawn p -> Some p.color
    | Knight p -> Some p.color
    | Bishop p -> Some p.color
    | Rook p -> Some p.color
    | Queen p -> Some p.color
    | King p -> Some p.color

  let tile_contains_piece t x y =
    match t.board.(x).(y) with Empty -> false | _ -> true

  let get_tile_color_exn t x y = get_tile_color t x y |> Option.value_exn

  let can_move_to_spot t x1 y1 x2 y2 =
    in_bounds x2 y2
    &&
    match (get_tile_color_exn t x1 y1, get_tile_color t x2 y2) with
    | _, None -> true
    | c1, Some c2 -> not (equal_color c1 c2)

  let get_sliding_moves t x y dx dy range =
    let moves =
      List.map
        (List.range 1 (range + 1))
        ~f:(fun i -> (x + (i * dx), y + (i * dy)))
    in
    List.take_while moves ~f:(fun (x2, y2) ->
        can_move_to_spot t x y x2 y2
        && not (can_move_to_spot t x y (x2 + dx) (y2 + dy)))

  let get_knight_moves t x y =
    List.filter
      [
        (x + 2, y + 1);
        (x + 2, y - 1);
        (x + 1, y + 2);
        (x + 1, y - 2);
        (x - 1, y + 2);
        (x - 1, y - 2);
        (x - 2, y + 1);
        (x - 2, y - 1);
      ]
      ~f:(fun (x2, y2) -> can_move_to_spot t x y x2 y2)
    |> List.map ~f:(fun m -> Move_or_attack ((x, y), m))

  let get_bishop_moves t x y =
    List.map
      [ (1, 1); (1, -1); (-1, 1); (-1, -1) ]
      ~f:(fun (dx, dy) -> get_sliding_moves t x y dx dy 8)
    |> List.concat
    |> List.map ~f:(fun m -> Move_or_attack ((x, y), m))

  let get_rook_moves t x y =
    List.map
      [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
      ~f:(fun (dx, dy) -> get_sliding_moves t x y dx dy 8)
    |> List.concat
    |> List.map ~f:(fun m -> Move_or_attack ((x, y), m))

  let get_queen_moves t x y = get_bishop_moves t x y @ get_rook_moves t x y

  let get_king_moves t x y =
    List.cartesian_product [ -1; 0; 1 ] [ -1; 0; 1 ]
    |> List.map ~f:(fun (dx, dy) -> (x + dx, y + dy))
    |> List.filter ~f:(fun (x2, y2) ->
           (x <> x2 || y <> y2) && can_move_to_spot t x y x2 y2)
    |> List.map ~f:(fun m -> Move_or_attack ((x, y), m))

  let get_pawn_moves t x y =
    let start_y, end_y, dy =
      match get_tile_color_exn t x y with
      | Black -> (6, 1, -1)
      | White -> (1, 6, 1)
    in
    if y = end_y then []
    else
      let attack_moves =
        get_sliding_moves t x y 1 dy 1 @ get_sliding_moves t x y (-1) dy 1
      in
      let first_move_forward =
        if tile_contains_piece t x (y + dy) then [] else [ (x, y + dy) ]
      in
      let second_move_forward =
        if
          List.is_empty first_move_forward
          || tile_contains_piece t x (y + (2 * dy))
          || y <> start_y
        then []
        else [ (x, y + (2 * dy)) ]
      in
      List.map attack_moves ~f:(fun m -> Attack ((x, y), m))
      @ List.map (first_move_forward @ second_move_forward) ~f:(fun m ->
            Move ((x, y), m))

  let get_promo_moves t x y =
    let end_y, dy =
      match get_tile_color_exn t x y with Black -> (1, -1) | White -> (6, 1)
    in
    if y <> end_y then []
    else
      let move_forward =
        if tile_contains_piece t x (y + dy) then [] else [ (x, y + dy) ]
      in
      let side_moves =
        List.filter
          [ (x + 1, y + dy); (x - 1, y + dy) ]
          ~f:(fun (x2, y2) ->
            match (get_tile_color_exn t x y, get_tile_color t x2 y2) with
            | _, None -> true
            | c1, Some c2 -> not (equal_color c1 c2))
      in
      let possible_squares = move_forward @ side_moves in
      List.fold possible_squares ~init:[] ~f:(fun acc (x2, y2) ->
          Promotion (To_knight, (x, y), (x2, y2))
          :: Promotion (To_bishop, (x, y), (x2, y2))
          :: Promotion (To_rook, (x, y), (x2, y2))
          :: Promotion (To_queen, (x, y), (x2, y2))
          :: acc)

  let get_enpassant_moves t x y =
    let can_enpassant x y =
      in_bounds x y
      && match t.board.(x).(y) with Pawn p -> p.en_passantable | _ -> false
    in
    let dy = match get_tile_color_exn t x y with Black -> -1 | White -> 1 in
    if can_enpassant (x + 1) y then
      [ En_passant ((x, y), (x + 1, y + dy), (x + 1, y)) ]
    else if can_enpassant (x - 1) y then
      [ En_passant ((x, y), (x - 1, y + dy), (x - 1, y)) ]
    else []

  let square_under_attack_by_color t x y color =
    let possible_moves =
      List.fold all_squares ~init:[] ~f:(fun acc (x, y) ->
          match get_tile_color t x y with
          | None -> acc
          | Some c ->
              if not (equal_color c color) then acc
              else
                (match t.board.(x).(y) with
                | Empty -> []
                | Pawn _ -> get_pawn_moves t x y @ get_promo_moves t x y
                | Knight _ -> get_knight_moves t x y
                | Bishop _ -> get_bishop_moves t x y
                | Rook _ -> get_rook_moves t x y
                | Queen _ -> get_queen_moves t x y
                | King _ -> get_king_moves t x y)
                @ acc)
    in
    List.exists possible_moves ~f:(fun m ->
        match m with
        | Move _ | Castle _ | En_passant _ -> false
        | Promotion (_, _, (x2, y2))
        | Move_or_attack (_, (x2, y2))
        | Attack (_, (x2, y2)) ->
            x = x2 && y = y2)

  let is_king_in_check t color =
    let x, y =
      List.find_exn all_squares ~f:(fun (x, y) ->
          match t.board.(x).(y) with
          | King k -> equal_color color k.color
          | _ -> false)
    in
    square_under_attack_by_color t x y color

  let can_castle t end_king_x rook_x y =
    let start_king_x = 4 in
    let pieces_havent_moved =
      match (t.board.(start_king_x).(y), t.board.(rook_x).(y)) with
      | King k, Rook r -> not (k.has_moved || r.has_moved)
      | _, _ -> false
    in
    let no_pieces_between =
      List.for_all
        (List.range
           (Int.min start_king_x rook_x + 1)
           (Int.max start_king_x rook_x))
        ~f:(fun x -> not (tile_contains_piece t x y))
    in
    let safe_path =
      List.for_all
        (List.range
           (Int.min start_king_x end_king_x)
           (Int.max start_king_x rook_x + 1))
        ~f:(fun x ->
          not (square_under_attack_by_color t x y (flip_color t.turn)))
    in
    pieces_havent_moved && no_pieces_between && safe_path

  let can_white_castle_kingside t = can_castle t 6 7 0
  let can_white_castle_queenside t = can_castle t 2 0 0
  let can_black_castle_kingside t = can_castle t 6 7 0
  let can_black_castle_queenside t = can_castle t 2 0 7

  let get_castle_moves t =
    match t.turn with
    | White ->
        (if can_white_castle_kingside t then [ Castle Kingside ] else [])
        @ if can_white_castle_queenside t then [ Castle Queenside ] else []
    | Black ->
        (if can_black_castle_kingside t then [ Castle Kingside ] else [])
        @ if can_black_castle_queenside t then [ Castle Queenside ] else []

  let apply_standard_move t ((start_x, start_y), (end_x, end_y)) =
    let new_board = Array.copy_matrix t.board in
    let new_piece =
      match new_board.(start_x).(start_y) with
      | Rook p -> Rook { p with has_moved = true }
      | Pawn p ->
          if Int.abs (start_y - end_y) = 2 then
            Pawn { p with en_passantable = true; has_moved = true }
          else Pawn { p with has_moved = true }
      | King p -> King { p with has_moved = true }
      | _ -> new_board.(start_x).(start_y)
    in
    new_board.(end_x).(end_y) <- new_piece;
    new_board.(start_x).(start_y) <- Empty;
    { board = new_board; turn = flip_color t.turn }

  let apply_castle t castle =
    let new_board = Array.copy_matrix t.board in
    let king_start = 4 in
    let rook_start, rook_end, king_end =
      match (t.turn, castle) with
      | White, Kingside | Black, Queenside -> (7, 5, 6)
      | White, Queenside | Black, Kingside -> (0, 3, 2)
    in
    let y = match t.turn with White -> 0 | Black -> 7 in
    let rook = Rook { color = t.turn; has_moved = true } in
    let king = King { color = t.turn; has_moved = true } in
    new_board.(rook_start).(y) <- Empty;
    new_board.(king_start).(y) <- Empty;
    new_board.(rook_end).(y) <- rook;
    new_board.(king_end).(y) <- king;
    { board = new_board; turn = flip_color t.turn }

  let apply_en_passant t ((x1, y1), (x2, y2), (destroy_x, destroy_y)) =
    let new_board = Array.copy_matrix t.board in
    new_board.(x2).(y2) <- new_board.(x1).(y1);
    new_board.(x1).(y1) <- Empty;
    new_board.(destroy_x).(destroy_y) <- Empty;
    { board = new_board; turn = flip_color t.turn }

  let apply_promotion t (promo, (x1, y1), (x2, y2)) =
    let new_board = Array.copy_matrix t.board in
    let new_piece =
      match promo with
      | To_knight -> Knight { color = t.turn }
      | To_bishop -> Bishop { color = t.turn }
      | To_queen -> Queen { color = t.turn }
      | To_rook -> Rook { color = t.turn; has_moved = true }
    in
    new_board.(x2).(y2) <- new_piece;
    new_board.(x1).(y1) <- Empty;
    { board = new_board; turn = flip_color t.turn }

  let apply_move t = function
    | Attack m | Move m | Move_or_attack m -> apply_standard_move t m
    | Castle m -> apply_castle t m
    | En_passant m -> apply_en_passant t m
    | Promotion m -> apply_promotion t m

  let get_allowed_moves t =
    List.fold all_squares ~init:(get_castle_moves t) ~f:(fun acc (x, y) ->
        match get_tile_color t x y with
        | None -> acc
        | Some c ->
            if not (equal_color c t.turn) then acc
            else
              (match t.board.(x).(y) with
              | Empty -> []
              | Pawn _ ->
                  get_pawn_moves t x y @ get_promo_moves t x y
                  @ get_enpassant_moves t x y
              | Knight _ -> get_knight_moves t x y
              | Bishop _ -> get_bishop_moves t x y
              | Rook _ -> get_rook_moves t x y
              | Queen _ -> get_queen_moves t x y
              | King _ -> get_king_moves t x y)
              @ acc)
    |> List.filter ~f:(fun m ->
           let new_board = apply_move t m in
           not (is_king_in_check new_board t.turn))

  let static_eval t =
    List.fold all_squares ~init:0. ~f:(fun acc (x, y) ->
        let multiplier =
          match get_tile_color t x y with
          | None -> 0.
          | Some Black -> -1.
          | Some White -> 1.
        in
        let value =
          match t.board.(x).(y) with
          | Empty -> 0.
          | Pawn _ -> 1.
          | Knight _ -> 3.
          | Bishop _ -> 3.
          | Rook _ -> 5.
          | Queen _ -> 9.
          | King _ -> 1000.
        in
        Float.(acc + (value * multiplier)))

  let game_over t = List.is_empty (get_allowed_moves t)
  let display _ = ()
end

include Board
