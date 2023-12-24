open Core

module Make_solver (Game : Game.Game) = struct
  type player = Player1 | Player2

  let rec minimax game depth alpha beta player : Game.move option * float =
    if depth = 0 || Game.is_over game then (None, Game.static_eval game)
    else
      match player with
      | Player1 ->
          let best_move, max_eval, _alpha =
            List.fold_until
              (Game.get_allowed_moves game)
              ~init:(None, Float.neg_infinity, alpha)
              ~f:(fun (best_move, max_eval, alpha) move ->
                let eval =
                  minimax
                    (Game.apply_move game move)
                    (depth - 1) alpha beta Player2
                  |> snd
                in
                let best_move, max_eval =
                  if Float.(max_eval >= eval) && Option.is_some best_move then
                    (best_move, max_eval)
                  else (Some move, eval)
                in
                let alpha = Float.max alpha eval in
                let res = (best_move, max_eval, alpha) in
                if Float.(beta <= alpha) then Continue_or_stop.Stop res
                else Continue res)
              ~finish:Fn.id
          in
          (best_move, max_eval)
      | Player2 ->
          let best_move, min_eval, _beta =
            List.fold_until
              (Game.get_allowed_moves game)
              ~init:(None, Float.infinity, beta)
              ~f:(fun (best_move, min_eval, beta) move ->
                let eval =
                  minimax
                    (Game.apply_move game move)
                    (depth - 1) alpha beta Player1
                  |> snd
                in
                let best_move, min_eval =
                  if Float.(min_eval <= eval) && Option.is_some best_move then
                    (best_move, min_eval)
                  else (Some move, eval)
                in
                let beta = Float.min beta eval in
                let res = (best_move, min_eval, beta) in
                if Float.(beta <= alpha) then Continue_or_stop.Stop res
                else Continue res)
              ~finish:Fn.id
          in
          (best_move, min_eval)

  let get_move game depth player =
    minimax game depth Float.neg_infinity Float.infinity player
end
