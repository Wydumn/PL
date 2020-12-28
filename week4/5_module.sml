signature RATIONAL_A =
sig
    datatype rational = Frac of int * int
                      | Whole of int
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

(*
    use signature for hideing
        1. deny bindings exist (val-bindings, fun-bindings, constructors)
        2. make types abstract (cannot create values of them or access their pieces directly)
*)

signature RATIONAL_B =
sig
    type rational
    exception BadFrac
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

signature RATIONAL_C =
sig
    type rational
    exception BadFrac
    val Whole : int -> rational
    val make_frac : int * int -> rational
    val add : rational * rational -> rational
    val toString : rational -> string
end

structure Rational1 :> RATIONAL_A =
struct
(* Invariant 1: all denominators > 0
    Invariant 2: rationals kept in reduced form, including that a Frac never have denominator of 1
*)
    datatype rational = Whole of int | Frac of int*int
    exception BadFrac

(* gcd and reduce help keep fractions reduced, but clients need not konw about them *)
    fun gcd (x,y) =
        if x = y
        then x
        else if x > y
        then gcd(y,x)
        else gcd(x,y-x)

    fun reduce r =
        case r of
            Whole _ => r
          | Frac(x,y) =>
            if x=0
            then Whole 0
            else let val d = gcd(abs x,y)
                 in
                    if d=y
                    then Whole(x div d)
                    else Frac(x div d, y div d)
                 end

    (* when making a frac, we ban zero denominators *)
    fun make_frac (x,y) =
        if y=0
        then raise BadFrac
        else if y < 0
        then reduce(Frac(~x,~y))
        else reduce(Frac(x,y))

    (* using math properties, both invariants hold of the result assuming they hold of the arguments *)
    fun add (r1,r2) =
        case (r1,r2) of
            (Whole(i),Whole(j)) =>  Whole(i+j)
          | (Whole(i),Frac(j,k))    =>  Frac(j+k*i,k)
          | (Frac(j,k),Whole(i))    =>  Frac(j+k*i,k)
          | (Frac(a,b),Frac(c,d))   =>  reduce (Frac(a*d+c*b, b*d))

    (* given invariant, prints in reduced form  *)
    fun toString r =
        case r of
            Whole i =>  Int.toString i
          | Frac(a,b)   =>  (Int.toString a) ^ "/" ^ (Int.toString b)
end

