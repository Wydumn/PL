fun map (f, xs) =
    case xs of
        []  =>  []
      | x::xs'  =>  (f x)::(map (f, xs'))

val x1 = map (fn x => x + 1, [1, 2, 3])

fun filter (f, xs) =
    case xs of
        []  =>  []
      | x::xs'  =>  if (f x)
                    then x::(filter (f, xs'))
                    else filter (f, xs')

fun get_all_even_snd xs = filter ((fn (_, x) => x mod 2 = 0), xs )