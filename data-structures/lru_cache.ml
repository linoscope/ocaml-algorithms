open Doubly_linked_list
open Core_kernel

module Make(Key: Hashtbl_intf.Key): sig
  type 'v t
  type key = Key.t
  val create: size_limit:int -> 'v t
  val add: 'v t -> key -> 'v -> unit
  val find: 'v t -> key -> 'v option
end = struct
  module H = Hashtbl.Make(Key)
  module Dll = Doubly_linked_list

  type key = Key.t

  type 'v t = {
    cache: (key * 'v) Dll.t;
    lookup_tbl: (key * 'v) Dll.node H.t;
    mutable size: int;
    size_limit: int;
  }

  let create ~size_limit = {
    cache = Dll.create ();
    lookup_tbl = H.create ();
    size = 0;
    size_limit
  }

  let evict_least_recent t =
    match Dll.remove_last t.cache with
      None -> ()
    | Some (key, _) ->
      H.remove t.lookup_tbl key

  let add ({cache; lookup_tbl; size; size_limit} as t) k v =
    t.size <- Int.min size_limit (size + 1);
    if size_limit < size + 1 then evict_least_recent t;
    match H.find lookup_tbl k with
      None ->
      let n = Dll.insert_first cache (k, v) in
      H.set lookup_tbl ~key:k ~data:n
    | Some n ->
      Dll.remove cache n;
      let n = Dll.insert_first cache (k, v) in
      H.set lookup_tbl ~key:k ~data:n

  let find {lookup_tbl; cache; _} k =
    match H.find lookup_tbl k with
      None -> None
    | Some old_node ->
      let (k, v) = Dll.node_value old_node in
      Dll.remove cache old_node;
      let new_node = Dll.insert_first cache (k, v) in
      H.set lookup_tbl ~key:k ~data:new_node;
      Some v
end

module LRU = Make(Int)

let equal = Option.equal Char.equal

let%test_unit "can lookup by key" =
  let lru = LRU.create ~size_limit:3 in
  LRU.add lru 1 'a';
  LRU.add lru 2 'b';
  LRU.add lru 3 'c';

  let actual = (LRU.find lru 1) in

  let expected = Some 'a' in
  assert (equal actual expected)

let%test_unit "evicts least recent element when over size limit" =
  let lru = LRU.create ~size_limit:2 in
  LRU.add lru 1 'a';
  LRU.add lru 2 'b';
  ignore (LRU.find lru 1 : char option);
  LRU.add lru 3 'c';

  let actual = (LRU.find lru 1) in

  let expected = Some 'a' in
  assert (equal actual expected);

  let actual = (LRU.find lru 2) in

  let expected = None in
  assert (equal actual expected)
