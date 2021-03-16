open Core_kernel

module ModInt = struct
  let modulo = 1_000_000_007
  let (+) x y = (x + y) mod modulo
  let ( * ) x y = (x * y) mod modulo
  let (-) x y = (x - y + modulo) mod modulo
  let rec (^) x n =
    if n = 0 then 1
    else if (n mod 2) = 1 then x * (x ^ Int.(n - 1))
    else let h = x ^ (n / 2) in h * h
end

module Combinations : sig
  val nCk:int -> int -> int
  val nHk: int -> int -> int
  (* fact_formula [a; b] [c; d] := !a!b/!c!d *)
  val fact_formula: int list -> int list -> int
end = struct
  open ModInt
  let len = 202020
  let inv_mod a = a ^ (modulo - 2)
  let facts =
    let res = Array.create ~len 1 in
    for i = 1 to len-1 do res.(i) <- i * res.(i-1); done;
    res
  let inv_facts = Array.map ~f:inv_mod facts
  let nCk n k =
    if n < k then 0 else facts.(n) * inv_facts.(k) * inv_facts.(n-k)
  let nHk n k = nCk (n + k - 1) k
  let fact_formula ns ds =
    let numer = List.fold ns ~init:1 ~f:(fun acc x -> facts.(x) * acc) in
    let denom = List.fold ds ~init:1 ~f:(fun acc x -> inv_facts.(x) * acc) in
    numer * denom
end

let%test_unit "test nCk" =
  assert ((Combinations.nCk 20 4) = 4845)

let%test_unit "test nHk" =
  assert ((Combinations.nHk 400 296) = 546898535)
