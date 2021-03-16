open Core_kernel

module type Monoid = sig
  type t
  val e : t
  val op: t -> t -> t
end

module SegTree(M: Monoid): sig
  type t
  val init: int -> t
  val of_list: M.t list -> t
  val update: t -> int -> M.t -> unit
  val query: t -> int -> int -> M.t
end = struct

  type t = { depth: int; width: int ; tree: M.t array }

  let parent i = i lsr 1
  let left i = i lsl 1
  let right i = i lsl 1 + 1

  let of_list (l: M.t list) =
    let size = List.length l in
    let depth = Int.ceil_log2 size in
    let width = Int.pow 2 depth in
    let tree = Array.create ~len:(2 * width) M.e in
    List.iteri l ~f:(fun i v -> tree.(i + width) <- v);
    for i = width - 1 downto 1 do
      tree.(i) <- M.op tree.(left i) tree.(right i)
    done;
    {depth; width; tree}

  let init n = of_list (List.init n ~f:(fun _ -> M.e))

  let update (t: t) (i:int) (v:M.t): unit =
    let i = i + (1 lsl t.depth) in
    t.tree.(i) <- v;
    let rec loop i =
      if i > 1 then
        let pi = parent i in
        t.tree.(pi) <- M.op t.tree.(left pi) t.tree.(right pi);
        loop pi
    in
    loop i

  let query (t: t) (a: int) (b: int) =
    let rec query_aux i l r =
      if (r <= a || b <= l) then M.e
      else if (a <= l && r <= b) then t.tree.(i)
      else
        let mid = (l + r) / 2 in
        let vl = query_aux (left i) l mid in
        let vr = query_aux (right i) mid r in
        M.op vl vr
    in
    query_aux 1 0 t.width
end

let%test "Range min query" =
  let module RMQ = SegTree(struct
      type t = int
      let e = Int.max_value
      let op = Int.min
    end)
  in
  let rmq = RMQ.init 5 in
  RMQ.update rmq 0 3;
  RMQ.update rmq 1 2;
  RMQ.update rmq 2 1;
  RMQ.update rmq 3 4;
  RMQ.update rmq 4 5;
  Int.(=) (RMQ.query rmq 2 4) 1
  && Int.(=) (RMQ.query rmq 1 2) 2
  && Int.(=) (RMQ.query rmq 3 5) 4

let%test "Range sum query" =
  let module RMQ = SegTree(struct
      type t = int
      let e = 0
      let op = ( + )
    end)
  in
  let rmq = RMQ.init 5 in
  RMQ.update rmq 0 3;
  RMQ.update rmq 1 2;
  RMQ.update rmq 2 1;
  RMQ.update rmq 3 4;
  RMQ.update rmq 4 5;
  Int.(=) (RMQ.query rmq 2 3) 1
  && Int.(=) (RMQ.query rmq 2 4) 5
  && Int.(=) (RMQ.query rmq 0 6) 15
