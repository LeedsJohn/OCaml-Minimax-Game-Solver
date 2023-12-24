open! Core
open Game_solver
module Solver = Solver.Make_solver (Connect4)

let _ =
  let rec aux game =
    Connect4.display game;
    if Connect4.is_over game then
      printf "Final eval: %f\n" (Connect4.static_eval game)
    else
      let player =
        if Connect4.get_turn game then Solver.Player1 else Solver.Player2
      in
      let move = Solver.get_move game 8 player |> fst in
      match move with
      | None -> ()
      | Some move -> aux (Connect4.apply_move game move)
  in
  aux Connect4.empty_board
