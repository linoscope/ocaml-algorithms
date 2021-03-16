open Core_kernel

module UnionFind: sig
  type 'a t
  val make: 'a -> 'a t
  val union: 'a t -> 'a t -> unit
  val same: 'a t -> 'a t -> bool
  val rep: 'a t -> 'a
  val size: 'a t -> int
end = struct

  type 'a t = ('a node) ref

  and 'a node =
      Link of 'a t
    | Root of {elem: 'a; rank: int; size: int}

  let equal t t' = phys_equal t t'

  let make elem = ref (Root {elem; rank = 1; size = 1})

  let rec find t =
    match !t with
    | Root _ -> t
    | Link parent ->
      let root = find parent in
      t := Link root;
      root

  let union t t' =
    let root = find t in
    let root' = find t' in
    if equal root root' then ()
    else
      match !root, !root' with
      | (Root {elem; rank; size}, Root {elem=elem'; rank=rank'; size=size'}) ->
        if rank > rank' then begin
          root' := Link root;
          root := Root {elem; rank; size = size + size'}
        end else if rank < rank' then begin
          root := Link root';
          root' := Root {elem = elem'; rank = rank'; size = size + size'}
        end else begin
          root' := Link root;
          root := Root {elem; rank = rank + 1; size = size + size'};
        end
      | (Link _, Root _) | (Root _, Link _) | (Link _, Link _) -> assert false

  let same t t' =
    equal (find t) (find t')

  let rep t = match !(find t) with
    | Root {elem; _} -> elem
    | Link _ -> assert false

  let size t = match !(find t) with
    | Root {elem=_; rank=_; size} -> size
    | Link _ -> assert false
end

let test_union_find_trees () =
  let ufs = Array.init 5 ~f:(fun _ -> UnionFind.make ()) in
  UnionFind.union ufs.(0) ufs.(1);
  UnionFind.union ufs.(1) ufs.(2);
  ufs

let%test "When elements are in the same set, same returns true" =
  let ufs = test_union_find_trees () in
  Bool.(=) (UnionFind.same ufs.(0) ufs.(2)) true

let%test "When elements are not in the same set, same returns false" =
  let ufs = test_union_find_trees () in
  Bool.(=) (UnionFind.same ufs.(0) ufs.(3)) false

let%test "Size returns size of the set" =
  let ufs = test_union_find_trees () in
  Int.(=) (UnionFind.size ufs.(0)) 3
