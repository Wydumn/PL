signature RATIONAL_B =
sig
    type rational
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end


structure Rational2 :> RATIONAL_A =
struct
(* Invariant 1: all denominators > 0
    Invariant 2: rationals kept in reduced form, including that a Frac never have denominator of 1
*)
    type rational = int*int
    exception BadFrac

(* gcd and reduce help keep fractions reduced, but clients need not konw about them *)


    fun make_frac (x,y) =
        if y=0
        then raise BadFrac
        else if y < 0
        then (~x,~y)
        else (x,y)

    fun add ((a,b),(c,d)) = (a*d + c*b, b*d)

    (* given invariant, prints in reduced form  *)
    fun toString (x,y) =
        if x = 0
        then "0"
        else
            let fun gcd (x,y) =
                    if x = y
                    then x
                    else if x > y
                    then gcd(y,x)
                    else gcd(x,y-x)
                val d = gcd (abs x, y)
                val num = x div d
                val denom = y div d
            in
                Int.toString num ^ (if denom=1
                                    then ""
                                    else "/" ^ (Int.toString denom))
            end
    fun Whole i = (i,1)
end