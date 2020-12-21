(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2
(*1a*)
fun all_except_option(s:string, l:string list) =
     case l of
	 [] => NONE
       | head::tail => if same_string(s,head)
		       then SOME tail
		       else case all_except_option(s,tail) of
				NONE => NONE
			      | SOME tl => SOME (head::tl)


val test1 = all_except_option("string",["zxc","qwe","string","asd"]) = SOME ["zxc","qwe","asd"]
(*1b*)
fun get_substitutions1(sl: string list list, s: string) =
  case sl of
      [] => []
    | head :: tail => case all_except_option(s,head) of
			  NONE => get_substitutions1(tail,s)
			| SOME segment => segment @ get_substitutions1(tail,s)


val test2 = get_substitutions1([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =
	    ["Fredrick","Freddie","F"]

(*1c*)
fun get_substitutions2(sl,s) =
  let fun aux(x , acc) =
	case x of
	    [] => acc
	  | head :: tail => case all_except_option(s, head) of
				NONE => aux(tail,acc)
			      | SOME xs => aux(tail, acc @ xs)
  in
      aux(sl,[])
  end

val test3 = get_substitutions2([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], "Fred") =
	    ["Fredrick","Freddie","F"]


(* 1d *)
fun similar_names (lst, name) =
    let val {first=x,middle=y,last=z} = name
      fun generate_names sub_lst =
	case sub_lst of
	    [] => []
	  | head::tail => {first=head,middle=y,last=z}::generate_names(tail)
  in
      name::generate_names(get_substitutions2(lst, x))
  end

val test4 = similar_names([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)

datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(suit,rank) =
  case suit of
      Clubs => Black
    | Spades => Black
    | Hearts => Red
    | Diamonds => Red

fun card_value(suit,rank) =
  case rank of
      Ace => 11
    | Jack => 10
    | Queen => 10
    | King => 10
    | Num n => n

(*remove_card can write to tail recursion*)
fun remove_card(cs, c, e) =
  case cs of
      [] => raise e
    | head :: tail => if (head = c)
		      then tail
		      else head :: remove_card(tail,c,e)

fun all_same_color(cs) =
  case cs of
      [] => true
    | _::[] => true
    | head::(neck::tail) => card_color(head) = card_color(neck)
			    andalso all_same_color(neck::tail)

(*sum_cards can write to tail recursion*)
fun sum_cards(cs) =
  case cs of
      [] => 0
    | c::[] => card_value(c)
    | head :: tail => card_value(head) + sum_cards(tail)

fun prescore(sum,goal) =
  if (sum > goal)
  then 3*(sum - goal)
  else (goal - sum)

fun score(cs,goal) =
  let val sum = sum_cards(cs)
  in
  if all_same_color(cs)
  then prescore(sum,goal) div 2
  else prescore(sum,goal)
  end


fun officiate(cs, mvs, goal) =
  let fun helper(card_list, move_list, held_cards) =
	case move_list of
	    [] => score(held_cards, goal)
	  | (Discard card):: tail =>
	    helper(card_list,tail,remove_card(held_cards,card,IllegalMove))
	  | (Draw):: tail => case card_list of
				 [] => score(held_cards,goal)
			       | first::rest => if sum_cards(first::held_cards) > goal
						then score(first::held_cards, goal)
						else helper(rest,tail,first::held_cards)
  in
      helper(cs,mvs,[])
  end