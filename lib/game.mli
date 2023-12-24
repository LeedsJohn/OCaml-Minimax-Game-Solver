open! Core

module type Game = sig
  type t [@@deriving sexp]
  type move [@@deriving sexp]

  val get_allowed_moves : t -> move list
  val apply_move : t -> move -> t

  (* returns a positive number if player 1 is winning and a negative number if
      player 2 is winning *)
  val static_eval : t -> float

  (* returns true if the game is in an ended position *)
  val is_over : t -> bool
  val display : t -> unit
end
