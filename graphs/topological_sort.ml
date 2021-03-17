open Core_kernel

module TopoLogicalSort (G: sig
    type t
    val size: t -> int
    val iter_adj: t -> from:int -> f:(int -> unit) -> unit
  end) = struct
  let sort (start: int) (g: G.t): int list =
    let visited = Array.create ~len:(G.size g) false in
    let sorted = Stack.create () in
    let rec dfs v =
      match visited.(v) with
        true -> ()
      | false ->
        visited.(v) <- true;
        G.iter_adj g ~from:v ~f:dfs;
        Stack.push sorted v
    in
    dfs start;
    Stack.to_list sorted
end

let%test_unit "test topological sort" =
  let module G = struct
    type t = unit
    let size _ = 5
    let iter_adj _ ~from:v ~f =
      List.iter ~f begin
        match v with
        | 1 -> [2; 3; 4]
        | 2 -> []
        | 3 -> []
        | 4 -> [3]
        | _ -> assert false
      end
  end
  in
  let module Topo = TopoLogicalSort(G) in

  let actual = Topo.sort 1 () in

  let expected = [1; 4; 3; 2] in
  let list_equal = List.equal Int.equal in
  assert (list_equal expected actual)
