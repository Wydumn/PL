(* anonymous functions *)
fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n-1, x))

fun triple x = 3 * x

fun triple_n_times (n, x) = n_times(triple, n, x)

(* we need triple just in n_times, move it inside function *)
fun triple_n_times (n, x) =
    let
        fun triple x = 3 * x
    in
        n_times(triple, n, x)
    end

(* and then *)
fun triple_n_times (n, x) =
    n_times(let fun triple x = 3 * x in triple end, n, x)

(* fn key word *)
fun triple_n_times (n, x) =
    n_times((fn y => 3 * y), n, x)


(* function binding is syntactic sugar, but support recursion *)

fun nth_tail (n, x) = n_times(tl, n, x)
