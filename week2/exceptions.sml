exception MyUndesirableCondition
exception MyOtherException of int * int

fun maxlist (xs, ex) =
    case xs of
        []  =>  raise ex
      | x::[] => x
      | x::xs'  =>  Int.max(x, maxlist(xs', ex))