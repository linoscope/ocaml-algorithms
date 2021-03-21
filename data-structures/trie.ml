open Core_kernel

module type TrieKey_intf = sig
  type t
  type fragment
  val to_list: t -> fragment list
end

module type Map_intf = sig
  type 'a t
  type key
  val set: 'a t -> key -> 'a -> 'a t
  val find: 'a t -> key -> 'a option
  val empty: 'a t
end

module type Trie_intf = sig
  type 'a t
  type key
  val empty: 'a t
  val insert: 'a t -> key -> 'a -> 'a t
  val search: 'a t -> key -> 'a option
end

module TrieTree
    (TrieKey: TrieKey_intf)
    (FragmentMap: Map_intf with type key := TrieKey.fragment)
  : Trie_intf with type key := TrieKey.t =
struct
  type 'a node = {
    value: 'a option;
    children: 'a node FragmentMap.t
  }

  type 'a t = 'a node

  let empty = { value = None; children = FragmentMap.empty }

  let insert t key value =
    let rec loop ({children; _} as node) fragments =
      match fragments with
        [] -> { value = Some value; children }
      | f::fs ->
        let next_node =
          match FragmentMap.find children f with
            None -> empty
          | Some n -> n
        in
        let new_children = FragmentMap.set children f (loop next_node fs) in
        { node with children = new_children }
    in
    let fragments = TrieKey.to_list key in
    loop t fragments

  let search t key =
    let rec loop {value; children} fragments =
      match fragments with
        [] -> value
      | f::fs ->
        match FragmentMap.find children f with
          None -> None
        | Some next_node -> loop next_node fs
    in
    let fragments = TrieKey.to_list key in
    loop t fragments
end

module TestTrieTree =
  TrieTree
    (struct
      type t = string
      type fragment = char
      let to_list = String.to_list
    end)
    (struct
      type 'a t = (char * 'a) list
      let empty = []
      let set t key v = (key, v)::t
      let find t key = List.Assoc.find t ~equal:(Char.equal) key
    end)

let%test_unit "if key value pair is inserted then that pair can be retireved later" =
  let t = TestTrieTree.empty in
  let t = TestTrieTree.insert t "abc" 1 in
  let t = TestTrieTree.insert t "acd" 2 in
  let t = TestTrieTree.insert t "bcd" 3 in
  assert Poly.((TestTrieTree.search t "abc") = (Some 1));
  assert Poly.((TestTrieTree.search t "acd") = (Some 2));
  assert Poly.((TestTrieTree.search t "bcd") = (Some 3))

let%test_unit "if no value is inserted for key then that key will return None when searched" =
  let t = TestTrieTree.empty in
  let t = TestTrieTree.insert t "abc" 1 in
  assert Poly.((TestTrieTree.search t "zzz") = None);
