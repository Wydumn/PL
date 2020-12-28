(* using earlier binding types to infer later *)


val x = 42
fun f (y,z,w) = if y then z+x else 0
(*
   f : T1 -> T2
   T1: T3 * T4 * T5
   y : T3=bool in if statement
   z : T4=int because + int * int -> int
   T2: int with 0 return

    bool * int * 'a -> int
*)

fun f x =
    let val (y,z) = x in
        (abs y) + z
    end

(*
    f : T1 -> T2
    x : T1=T3 * T4
    y : T3=int for abs: int -> int
    z : T4=int for +: int * int -> int
    +: int * int -> int so let-expression type is int so return type is int => T2 is int
 *)

fun sum xs =
    case xs of
        []  =>  0
      | x::xs' => x + (sum xs')

(*
    sum : T1 -> T2
    xs : T1=T3 list with case pattern
    T2=int  case branch has same type
    x::xs' T3 List
    x  : T3=int for +: int * int -> int so T2=int

    int list -> int
*)

fun broken_sum xs =
    case xs of
        [] => 0
      | x::xs'  =>  x + (broken_sum x)

(*
    x must have the same type as broken_sum's parameter
    that's T3 = T3 list

    Error: case object and rules do not agree [overload - bad instantiation]
  rule domain: 'Z[INT] list
*)

