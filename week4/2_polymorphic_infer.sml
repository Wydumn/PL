fun length xs =
    case xs of
        []  =>  0
      | x::xs'  =>  1 + (length xs')

(*
    length : T1 -> T2
    xs : T1
    T2 : int because case branch is same type 0
    T1 : T3 list
    x  : T3
    xs': T3 list

    ** type-checks xs' because we recursive call length xs'
        so xs' should be T1，and we already know xs' is T3 list, which is equivalent to T1

    'a list -> int
 *)

(* for each Ti in the final result that cannot be constrained, use a type variable  *)

fun compose (f,g) = fn x => f (g x)

(* 
    = : T1 * T2 -> T3
    fn : T3=T4 -> T5
    x : T4
    g : T4 -> T6
    f : T6 -> T7
    T7 = T5
    (T6 -> T7) * (T4 -> T6) -> (T4 -> T7)
    ('a -> 'b) * ('c -> 'a) -> ('c -> 'b)
*)

fun f (x,y,z) = 
    if true
    then (x,y,z)
    else (y,x,z)
(* 
    f : T1 * T2 * T3 -> T4
    then : T4=T1 * T2 * T3
    else : T4=T2 * T1 * T3
    then else same type T1 = T2
    T1 * T1 * T3 -> T4
    'a * 'a * 'b -> 'a * 'a * 'b
*)