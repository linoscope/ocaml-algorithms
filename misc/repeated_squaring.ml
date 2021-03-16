open Core_kernel

module RepeatedSquaring
    (M: sig
       type t
       val e: t
       val times: t -> t -> t
     end)
= struct
  let rec pow (x: M.t) (n: int): M.t =
    if n = 0 then M.e
    else if n % 2 = 0 then
      pow (M.times x x) (n / 2)
    else
      M.times (pow x (n - 1)) x
end
let%test_unit "repeated squaring of int" =
  let module RSI = RepeatedSquaring(struct
      let m = 1000_000_007
      type t = int
      let e = 1
      let times x y = (x * y) mod m
    end)
  in
  assert(RSI.pow 2 10 = 1024)

let%test_unit "repeated squaring of matirx" =
  let module RSM = RepeatedSquaring(struct
      let m = 1000_000_007
      type t = int array array
      let e = [| [| 1;0 |];
                 [| 0;1 |] |]
      let times m1 m2 =
        assert (Array.length m1 = Array.length m2);
        let res_m = Array.make_matrix ~dimx:2 ~dimy:2 0 in
        for i = 0 to Array.length m1 - 1 do
          for k = 0 to Array.length m2 - 1 do
            for j = 0 to Array.length m2.(0) - 1 do
              res_m.(i).(j) <- (res_m.(i).(j) + m1.(i).(k) * m2.(k).(j)) % m
            done
          done
        done;
        res_m
    end)
  in
  (* solve fibonacci sequence using matrix form *)
  let a = [| [| 1; 1 |];
             [| 1; 0|] |]
  in
  let b = RSM.pow a 10 in
  assert(b.(1).(0) = 55)
