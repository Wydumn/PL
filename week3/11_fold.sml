val foo = [1, 2, 3, 4]
fun fold (f, acc, xs) =
    case xs of
      []  =>  acc
    | x::xs'  =>  fold (f, f(acc, x), xs')

fun numberInRange (xs, lo, hi) =
    fold ((fn (x, y)  =>
              x + (if y >= lo andalso y <= hi then 1 else 0)),
          0, xs)

fun areAllShorter (xs, s) =
    let
        val i = String.size s
    in
        fold((fn (x, y) => x andalso String.size y < i), true, xs)
    end

(* do all elements of the list can pass function g *)
fun f5 (g, xs) = fold(fn (x, y) => x andalso g y, true, xs)

fun areAllShorter (xs, s) =
    let
        val i = String.size s
    in
        f5(fn y => String.size y < i, xs)
    end

(* why iterators again *)
(*
    - iterator-like function is just a programming pattern

    This pattern separate recursive traversal from data processing
    1. can reuse same traversal for different data processing
    2. can reuse same data processing for different data structures

 *)