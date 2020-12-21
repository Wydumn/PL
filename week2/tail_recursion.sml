fun sum1 xs =
    case xs of
        []  =>  0
      | i::xs'  =>  i + sum1 xs'

fun sum2 xs =
    let fun f (xs, acc) =
            case xs of
                []  =>  acc
              | i::xs'  =>  f(xs', i+acc)
    in
        f(xs, 0)
    end

(* why tail recursion is efficient?
      functional languages like ML typically promise an essential optimization: When
a call is a tail call, the caller’s stack-frame is popped before the call — the callee’s stack-frame just replaces
the caller’s. This makes sense: the caller was just going to return the callee’s result anyway. Therefore, calls
to sum2 never use more than 1 stack frame.
编译器的优化：被调函数进栈时，调用函数出栈，所以调用栈中最多就1个栈帧

Tail calls do not need to be to the same function (f can call g), so they are more flexible than while-loops that
always have to “call” the same loop. Using an accumulator is a common way to turn a recursive function
into a “tail-recursive function” (one where all recursive calls are tail calls), but not always. For example,
functions that process trees (instead of lists) typically have call stacks that grow as big as the depth of a
tree, but that’s true in any language: while-loops are not very useful for processing trees.
*)

fun fact1 n = if n = 0 then 1 else n * fact1(n-1)

fun fact2 n =
    let fun aux(n, acc) = if n = 0 then acc else aux(n-1, acc*n)
    in
        aux(n, 1)
    end

(* We are relying on the fact that multiplicationis associative (a∗(b∗c) = (a∗b)∗c) and that multiplying by 1 is the identity function (1∗x=x∗1 =x).  Theearliersumexample made analogous assumptions about addition.  In general, converting a non-tail-recursivefunction to a tail-recursive function usually needs associativity, but many functions are associative *)


fun rev1 lst =
    case lst of
        [] => []
      | x::xs => (rev1 xs) @ [x]

fun rev2 lst =
    let fun aux(lst, acc) =
            case lst of
                []  =>  acc
              | x::xs =>  aux(xs, x::acc)
    in
        aux(lst, [])
    end
(* 不占内存的计算就在参数中 *)

