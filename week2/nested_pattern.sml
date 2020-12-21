fun unzip3 lst =
  case lst of
      [] => ([], [], [])
    | (x, y, z)::tl => let val (l1, l2, l3) = unzip3 tl
                       in
                          (x::l1, y::l2, z::l3)
                       end

fun nondecreasing intlist =
    case intlist of
        [] => true
      | _::[] => true
      | head :: (neck :: rest) => head <= neck andalso nondecreasing (neck::rest)

datatype sgn = P | N | Z

fun multsign (factor1, factor2) =
  let fun sign x = if x > 0 then P else if x < 0 then N else Z
  in
      case (sign factor1, sign factor2) of
          (Z, _)  => Z
        | (_, Z)  => Z
        | (N, P)  => N
        | (P, N)  => N
        | _ => P
  end

datatype exp = Constant of int
              | Negate of exp
              | Add of exp * exp
              | Multiply of exp * exp
(* eval pattern match *)
(* fun eval e =
    case e of
        Constant i => i
      | Negate e2 =>  ~ (eval e2)
      | Add(e1, e2) => (eval e1) + (eval e2)
      | Multiply(e1, e2)  =>  (eval e1) * (eval e2) *)

(* eval with syntactic sugar *)
fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1, e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1, e2)) = (eval e1) * (eval e2)

(* fun append e =
    case e of
        ([], ys)  =>  ys
      | (x::xs', ys)  =>  x ::append(xs', ys) *)

fun append ([], ys) = ys
  | append (x::xs', ys) = x :: append(xs', ys)

fun hd xs =
  case xs of
      []  =>  raise List.Empty
    | x::_ => x
