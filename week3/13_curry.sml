(* 1. defining and using a curried function *)
val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

fun sorted3_tupled (x, y, z) = z >= y andalso y >= x

(* val wrong1 = sorted3 (7, 9, 11)
val wrong2 = sorted3_tupled 7 9 11 *)

val v1 = sorted3 7 9 11
(* (((sorted3 7) 9) 11) *)
(*
    1. calling (sorted3 7) returns a closure with:
        - fn y => fn z => z >= y andalso y >= x
        - Environment maps x to 7

    2. calling that closure with 9 returns a closure with:
        - fn z => z >= y andalso y >= x
        - Environment maps y to 9

    3. calling that closure with 11 returns true

 *)
(*
    1. 语法糖1
        e1 e2 e3 e4 就是 (((e1 e2) e3) e4)

        callers can just think "multi-argument function with spaces instead of a tuple expression"
        但是这样，caller和callee必须保持一致

    2. 语法糖2
        fun f p1 p2 p3 ... = 3 相当于 fun f p1 = fn p2 => fn p3 => ... => e

        所以上面的代码也可以写成 fun sorted3_with_syntactic_sugar x y z = z >= y andalso y >= x
 *)

val v2 = sorted3_tupled (7, 9, 11)

fun sorted3_with_syntactic_sugar x y z = z >= y andalso y >= x

val is_nonnegative = sorted3 0 0

fun fold f acc xs =
    case xs of
        []  =>  []
      | x::xs'  =>  fold f (f(acc,x)) xs'

val sum = fold (fn (x, y) => x+y) 0

fun sum_inferior xs = fold (fn (x,y) => x+y) 0 xs

fun curry_fold f acc xs =
    case xs of
        []  =>  acc
      | x::xs'  =>  curry_fold f (f(acc, x)) xs'

fun curry_sum xs = curry_fold (fn (x, y) => x + y) 0 xs

fun exists predicate xs =
    case xs of
        []  =>  false
      | x::xs'  =>  predicate x orelse exists predicate xs'

val no = exists (fn x => x > 5) [0, 1, 2, 3]
val hasZero = exists (fn x => x = 0)
val incrementAll = List.map (fn x => x+1)
val removeZeros = List.filter (fn x => x <> 0)

(* value restriction *)
val pairWithOne = List.map (fn x => (x, 1))
fun pairWithOne1 xs = List.map ((fn x => (x, 1))) xs
val pairWithOne2 : string list -> (string * int) list = List.map (fn x => (x, 1))



fun zip xs ys =
    case (xs, ys) of
        ([], [])    =>  []
      | (x::xs', y::ys')    =>  (x, y)::(zip xs' ys')
      | _ => raise Empty

fun range i j = if i > j then [] else i :: range (i+1) j

val countup = range 1
fun countup_inferior x = range 1 x

fun add_numbers xs = zip (countup (length xs)) xs

fun other_curry1 f = fn x => fn y => f y x
fun other_curry2 f x y = f y x

fun curry f x y = f (x, y)
(* fun curry f = fn x => fn y => f(x,y) *)
fun uncurry f (x, y) = f x y

(* val countupWithCurry = curry range 1 *)
(*
    logic formula
        -> imply
        * and
*)
