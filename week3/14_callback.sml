(*
    1. mutation
        alias or identical copy
        mutable：
        immutable：identical copy
*)

val x = ref 42
val y = ref 42
val z = x
val _ = x :=43
val w = !y + !z

(* a variable bound to a reference is still immutable:
    1. it always refer to the same reference
    2. but contents of reference may change

    reference is first-class value
*)

val cbs : (int -> unit) list ref = ref []
fun onKeyEvent f = cbs := f::(!cbs)     (*  *)
fun onEvent i =
    let fun loop fs =
            case fs of
                []  =>  ()
              | f::fs'  =>  (f i; loop fs')
    in loop (!cbs) end


val timesPressed = ref 0
val _ = onKeyEvent (fn _ => timesPressed := (!timesPressed) + 1)

fun printIfPressed i =
    onKeyEvent (fn j => if i = j
                        then print ("you pressed " ^ Int.toString i ^ "\n")
                        else ())

val _ = printIfPressed 4
val _ = printIfPressed 11
val _ = printIfPressed 23
    