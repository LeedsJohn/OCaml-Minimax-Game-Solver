type t [@@deriving sexp]
type move = int * int * char [@@deriving sexp]

val empty_board : t
val board_of_string : string -> t
val get_allowed_moves : t -> move list
val apply_move : t -> move -> t
val static_eval : t -> float
val is_over : t -> bool
val get_turn : t -> bool
(* TODO: change this so that it returns Player1 or Player2 instead of a boolean *)

val display : t -> unit
