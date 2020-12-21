(* lexical scope：use environment where function is defined *)
(* dynamic scope：use environment where function is called *)

(* 1. function meaning does not depend on variable names used *)
(* can replace x with q anywhere in body of function f *)
fun f1 y =
    let val x = y + 1
    in
        fn z => x + y + z
    end

fun f2 y =
    let val q = y + 1
    in
        fn z => q + y + z
    end

val x = 17
val a1 = (f1 7) 4
val a2 = (f2 7) 4
(* 2. Functions can be type-checked and reasoned about where defined *)
(* 3. Closures can easily store the data they need *)

(*
    exception handling is more like dynamic scope:
    - raise e transfers control to the current innermost handler(where it iscalled
 *)
