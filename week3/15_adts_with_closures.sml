(* type set = { insert: int -> set,
                  member: int -> bool,
                  size: unit -> int} *)
datatype set = S of { insert: int -> set,
                      member: int -> bool,
                      size  : unit -> int}

(* val empty_set : set *)

val empty_set =
    let
        fun make_set xs =
            let
                fun contains i = List.exists (fn j => i=j) xs
            in
                S { insert = fn i => if contains i
                                     then make_set xs
                                     else make_set (i::xs),
                    member = contains,
                    size   = fn ()  => length xs
                }
            end
    in
        make_set []
    end

(* example client *)
fun use_sets () =
    let val S s1 = empty_set
        val S s2 = (#insert s1) 34  (* s1.insert(34) OOP *)
        val S s3 = (#insert s2) 34
        val S s4 = #insert s3 19    (* 19, 34 *)
    in
        if (#member s4) 42
        then 99
        else if (#member s4) 19
        then 17 + (#size s3) ()
        else 0
    end