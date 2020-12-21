(* 1 *) val x = 1
        (* x maps to 1 *)
(* 2 *) fun f y = x + y
        (* function f increment 1 for whatever argument*)
(* 3 *) val x = 2
(* 4 *) val y = 3
(* 5 *) val z = f (x + y)
        (* call the second function with argument 2 + 3 ->  f (5)
           increment 6 for argument *)

(* 第二行定义了函数 f，evaluates body x + y in environment where x maps to 1
                       and y maps to the argument *)
(* 第五行调用
    1. look up f to get the funciton defined on line 2
    2. 在current environment中计算函数体x + y, 5
    3. call the function with 5, 当在old environment中计算函数体时，6
 *)

(* Closure *)
(* the semantics of functions
    - code
    - environment that was current when and where the function is defined
 *)
(*
  the two parts is what you called when you call a function
  这就是函数的语义，也就是函数闭包
  you can not access the part of the semantics separately,
  A call evaluates the code part in the environment part (extended with the function argument)
 *)