open Core_kernel

let inf = (Int.max_value / 2)

module WarshallFloyd = struct
  type result = NegativCycle
              | Dists of int array array

  let shortest_dists (adj_matrix: int array array): result =
    let dp = Array.copy adj_matrix in
    let n = Array.length adj_matrix in
    for i = 0 to (n - 1) do
      dp.(i).(i) <- 0
    done;
    for k = 0 to (n - 1) do
      for i = 0 to (n - 1) do
        for j = 0 to (n - 1) do
          dp.(i).(j) <- Int.min dp.(i).(j) (dp.(i).(k) + dp.(k).(j))
        done
      done
    done;
    let has_negative_cycle = Array.existsi dp ~f:(fun i row -> row.(i) < 0) in
    if has_negative_cycle then
      NegativCycle
    else
      Dists dp
end

let%test_unit "Calculates shortest paths" =
  let adj_matrix =
    [|
      [|0; 1; 5; inf|];
      [|inf; 0; 2; 4|];
      [|inf; inf; 0; 1|];
      [|inf; inf; 7; 0|];
    |]
  in

  let expected = [|
    [|0; 1; 3; 4|];
    [|inf; 0; 2; 3|];
    [|inf; inf; 0; 1|];
    [|inf; inf; 7; 0|]; |]
  in

  let n = Array.length adj_matrix in
  match WarshallFloyd.shortest_dists adj_matrix with
    WarshallFloyd.NegativCycle -> assert false
  | WarshallFloyd.Dists dists ->
    for i = 0 to (n - 1) do
      for j = 0 to (n - 1) do
        assert(Int.(=) dists.(i).(j) expected.(i).(j))
      done;
    done
