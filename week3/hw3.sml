exception NoAnswer

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)
(* 1 *)
fun only_capitals xs =
    List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

(* 2 *)
fun longest_string1 xs =
    foldl (fn (x, init) => if String.size(x) > String.size(init) then x else init) "" xs

(* 3 *)
fun longest_string2 xs =
    foldl (fn (x, init) => if String.size(x) >= String.size(init) then x else init) "" xs

(* 4 *)
fun longest_string_helper f = fn xs => foldl (fn (a, b) =>
                                                if f (String.size(a), String.size(b))
                                                then a
                                                else b
                                            )
                                            ""
                                            xs

val longest_string3 = fn xs => longest_string_helper (fn (a, b) => if a > b then true else false) xs

val longest_string4 = fn xs => longest_string_helper (fn (a, b) => if a >= b then true else false) xs

(* 5 *)
fun longest_capitalized xs = (longest_string1 o only_capitals) xs

(* 6 *)
fun rev_string s = (String.implode o List.rev o String.explode) s

(* 7 *)
(*

 *)
fun first_answer f = fn xs => case xs of
                                    [] => raise NoAnswer
                                |   x::xs'  =>  case (f x) of
                                                    SOME x => x
                                                  | NONE => first_answer f xs'

(* 8 *)
fun all_answers f = fn xs =>
                        let
                            fun helper (acc, lst) =
                                case lst of
                                    []  =>  SOME []
                                  | l::lst'  => case (f l) of
                                                    SOME l  =>  helper (acc @ [l], lst')
                                                  | NONE => NONE
                        in
                            helper ([], xs)
                        end

datatype pattern =
           Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu =
            Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let
	    val r = g f1 f2
    in
        case p of
            Wildcard          => f1 ()
          | Variable x        => f2 x
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | ConstructorP(_,p) => r p
          | _                 => 0
    end

(* 9 *)
(* a *)
(*
    Use g to define a function count_wildcards that takes a pattern and returns how many Wildcard
patterns it contains.
 *)
fun count_wildcards p = g (fn _ => 1) (fn _ => 0) p

(* b *)
fun count_wild_and_variable_lengths p = g (fn _ => 1) String.size p

(* c *)
fun count_some_var (s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
(*
    Write a function check_pat that takes a pattern and returns true if and only if all the variables
appearing in the pattern are distinct from each other (i.e., use di↵erent strings). The constructor
names are not relevant. Hints: The sample solution uses two helper functions. The first takes a
pattern and returns a list of all the strings it uses for variables. Using foldl with a function that
uses append is useful in one case. The second takes a list of strings and decides if it has repeats.
List.exists may be useful.
 *)
fun check_pat p =
    let fun helper pa =
            case pa of
                  Wildcard          => f1 ()
                | Variable x        => x
                | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
                | ConstructorP(_,p) =>
                | _                 =>
    in

    end


(* 11 *)

(* 12 *)