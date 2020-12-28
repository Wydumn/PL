(* val r = ref NONE
val _ = r := SOME "hi"
val i = 1 + valOf(!r) *)
type 'a foo = 'a ref
val f : 'a -> 'a foo = ref
val r = f NONE