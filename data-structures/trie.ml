open Core_kernel

module type Map = sig
  type key
  type 'a t
  val empty : 'a t
  val find : 'a t -> key:key -> 'a option
  val set : 'a t -> key:key -> data:'a -> 'a t
end

module Make (M : Map) : Map with type key = M.key list = struct
  type key = M.key list

  type 'a t = Node of 'a option * 'a t M.t

  let empty = Node (None, M.empty)

  let rec find t ~key =
    match key, t with
    | [], Node (None, _) -> None
    | [], Node (Some x, _) -> Some x
    | x::xs, Node (_, m) ->
      M.find m ~key:x
      |> Option.bind ~f:(fun t' -> find t' ~key:xs)

  let rec set t ~key ~data =
    match key, t with
    | [], Node (_, m) -> Node (Some data, m)
    | x::xs, Node (y, m) ->
      match M.find m ~key:x with
      | None -> Node (y, M.set m ~key:x ~data:(set empty ~key:xs ~data))
      | Some t' -> Node (y, M.set m ~key:x ~data:(set t' ~key:xs ~data ))
end


module Assoc_list_make (Key : Comparable.S) : Map with type key = Key.t = struct
  type key = Key.t
  type 'a t = (key * 'a) list
  let empty = []
  let rec find t ~key = match t with
    | [] -> None
    | (k, v)::xs -> if Key.(key = k) then Some v else find xs ~key
  let set t ~key ~data = (key, data)::t
end

module Trie = Make (Assoc_list_make (Char))

let%expect_test "test" =
  let t =
    Trie.empty
    |> Trie.set ~key:(String.to_list "abc") ~data:1
    |> Trie.set ~key:(String.to_list "abd") ~data:2
    |> Trie.set ~key:(String.to_list "abce") ~data:3
    |> Trie.set ~key:(String.to_list "xyz") ~data:4
  in

  ["abc"; "abd"; "abce"; "xyz"]
  |> List.map ~f:String.to_list
  |> List.map ~f:(fun k -> Trie.find t ~key:k)
  |> List.sexp_of_t (fun x -> Option.sexp_of_t (fun y -> Int.sexp_of_t y) x)
  |> Sexp.to_string
  |> print_endline;

  [%expect {| ((1)(2)(3)(4)) |}]
