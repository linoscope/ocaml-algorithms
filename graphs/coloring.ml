open Core_kernel

module Coloring
    (G: sig
       type t
       val size: t -> int
       val iter_adj: t -> from:int -> f:(int -> unit) -> unit
     end)
= struct
  type result = Colors of bool option array | NoColoring

  let color (g:G.t): result =
    let colors = Array.create ~len:(G.size g) None in
    let exception Exit in
    let rec color_exn v (c: bool) =
      match colors.(v) with
      | Some c' -> if Bool.(c = c') then raise Exit
      | None ->
        colors.(v) <- Some (not c);
        G.iter_adj g ~from:v ~f:(fun v' -> color_exn v' (not c))
    in
    try
      color_exn 0 true;
      Colors colors
    with
    | Exit -> NoColoring
end

let%test_unit "Detects graph with coloring" =
  let module G = struct
    type t = unit
    let size _ = 3
    let iter_adj _ ~from ~f =
      List.iter ~f (
        match from with
        | 0 -> [1]
        | 1 -> [2]
        | 2 -> [1]
        | _ -> assert false
      )
  end in
  let module C = Coloring(G) in
  match C.color () with
  | C.NoColoring -> assert false
  | C.Colors _   -> assert true

let%test_unit "Detects graph with no coloring" =
  let module G = struct
    type t = unit
    let size _ = 3
    let iter_adj _ ~from ~f =
      List.iter ~f (
        match from with
        | 0 -> [1]
        | 1 -> [2]
        | 2 -> [0]
        | _ -> assert false
      )
  end in
  let module C = Coloring(G) in
  match C.color () with
  | C.NoColoring -> assert true
  | C.Colors _   -> assert false
