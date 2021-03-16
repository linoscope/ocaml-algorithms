open Core_kernel

module type CommutativeMonoid = sig
  type t
  val e: t
  val op: t -> t -> t
end
module BinaryIndexTree(CM: CommutativeMonoid): sig
  type t
  val make: int -> t
  (* 0 index *)
  val add: t -> int -> CM.t -> unit
  val of_list: CM.t list -> t
  (* [0, x) *)
  val sum: t -> int -> CM.t
end = struct
  type t = CM.t array

  let make n = Array.create ~len:(n + 1) CM.e

  let add t i x =
    let rec add_aux i x =
      if i < Array.length t then begin
        t.(i) <- CM.op x t.(i);
        add_aux (i + (i land (-i))) x
      end
    in
    assert (i >= 0 && i + 1 < (Array.length t));
    add_aux (i + 1) x

  let of_list l =
    let t = make (List.length l) in
    List.iteri l ~f:(fun i x -> add t i x);
    t

  let sum t i =
    let rec sum_aux acc i =
      if i <= 0 then acc
      else
        sum_aux (CM.op acc t.(i)) (i - (i land (-i)))
    in
    assert (i >= 0);
    sum_aux CM.e i
end

let%test_unit "Range sum" =
  let module BIT = BinaryIndexTree(struct
      type t = int
      let e = 0
      let op = ( + )
    end)
  in
  let ft = BIT.make 3 in
  BIT.add ft 0 1;
  BIT.add ft 1 2;
  BIT.add ft 2 3;
  assert (Int.(=) (BIT.sum ft 3) 6)

let%test_unit "Range min" =
  let module BIT = BinaryIndexTree(struct
      type t = int
      let e = Int.max_value
      let op = Int.min
    end)
  in
  let ft = BIT.make 3 in
  BIT.add ft 0 3;
  BIT.add ft 1 1;
  BIT.add ft 2 2;
  assert (Int.(=) (BIT.sum ft 3) 1)
