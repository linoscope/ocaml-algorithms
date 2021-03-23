open Core_kernel

module Doubly_linked_list: sig
  type 'a node
  type 'a t
  val create: unit -> 'a t
  val insert_after: 'a t -> 'a node -> 'a -> 'a node
  val insert_before: 'a t -> 'a node -> 'a -> 'a node
  val insert_first: 'a t -> 'a -> 'a node
  val insert_last: 'a t -> 'a -> 'a node
  val remove: 'a t -> 'a node -> unit
  val fold: 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
  val iter: 'a t -> f:('a -> unit) -> unit
  val to_list: 'a t -> 'a list
end = struct
  type 'a node = {
    mutable prev: 'a node option;
    mutable next: 'a node option;
    value: 'a
  }

  type 'a t = {
    mutable first: 'a node option;
    mutable last: 'a node option
  }

  let create_node v = { prev = None; next = None; value = v }

  let create () = { first = None; last = None }

  let insert_after t n v =
    let new_node = create_node v in
    new_node.prev <- Some n;
    begin match n.next with
        None ->
        t.last <- Some new_node;
      | Some old_next ->
        old_next.prev <- Some new_node
    end;
    new_node

  let insert_before t n v =
    let new_node = create_node v in
    new_node.next <- Some n;
    begin match n.prev with
        None ->
        t.first <- Some new_node;
      | Some old_prev ->
        old_prev.next <- Some new_node
    end;
    new_node


  let insert_first t v =
    let new_first = create_node v in
    begin match t.first with
      | None ->
        t.first <- Some new_first;
        t.last  <- Some new_first
      | Some old_first ->
        old_first.prev <- Some new_first;
        new_first.next <- Some old_first;
        t.first <- Some new_first
    end;
    new_first

  let insert_last t v =
    let new_last = create_node v in
    begin match t.last with
      | None ->
        t.last <- Some new_last;
        t.first  <- Some new_last
      | Some old_last ->
        old_last.next <- Some new_last;
        new_last.prev <- Some old_last;
        t.last <- Some new_last
    end;
    new_last

  let remove t n =
    match n.prev, n.next with
      None, None ->
      t.first <- None;
      t.last  <- None;
    | Some pn, None ->
      pn.next <- None;
      t.last <- Some pn
    | None, Some nn ->
      nn.prev <- None;
      t.first <- Some nn
    | Some pn, Some nn ->
      pn.next <- Some nn;
      nn.prev <- Some pn

  let iter t ~f =
    let rec loop n_op =
      match n_op with
        None -> ()
      | Some n ->
        f n.value;
        loop n.next
    in
    loop t.first

  let fold t ~init ~f =
    let rec loop acc n_op =
      match n_op with
        None -> acc
      | Some n ->
        loop (f acc n.value) n.next
    in
    loop init t.first

  let to_list t =
    fold t ~init:[] ~f:(fun acc v -> v::acc)
    |> List.rev
end


let%test_unit "insert first" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  ignore (Dll.insert_first dll 1 : 'a Dll.node);
  ignore (Dll.insert_first dll 2 : 'a Dll.node);

  assert (List.equal (=) [2; 1] (Dll.to_list dll))

let%test_unit "insert last" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  ignore (Dll.insert_last dll 1 : 'a Dll.node);
  ignore (Dll.insert_last dll 2 : 'a Dll.node);

  assert (List.equal (=) [1; 2] (Dll.to_list dll))

let%test_unit "remove middle" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  ignore (Dll.insert_first dll 1 : 'a Dll.node);
  let mid_node = Dll.insert_first dll 2 in
  ignore (Dll.insert_first dll 3 : 'a Dll.node);
  Dll.remove dll mid_node;

  assert (List.equal (=) [3; 1] (Dll.to_list dll))

let%test_unit "remove first" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  ignore (Dll.insert_first dll 1 : 'a Dll.node);
  ignore (Dll.insert_first dll 2 : 'a Dll.node);
  let first_node = Dll.insert_first dll 3 in
  Dll.remove dll first_node;

  assert (List.equal (=) [2; 1] (Dll.to_list dll))

let%test_unit "remove last" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  let last_node = Dll.insert_first dll 1 in
  ignore (Dll.insert_first dll 2 : 'a Dll.node);
  ignore (Dll.insert_first dll 3 : 'a Dll.node);
  Dll.remove dll last_node;

  assert (List.equal (=) [3; 2] (Dll.to_list dll))

let%test_unit "remove singleton" =
  let module Dll = Doubly_linked_list in
  let dll = Dll.create () in
  let single_node = Dll.insert_first dll 1 in
  Dll.remove dll single_node;

  assert (List.equal (=) [] (Dll.to_list dll))
