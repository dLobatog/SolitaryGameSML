(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun all_except_option (str,[]) = NONE
  | all_except_option (str, x :: xs) = 
    case same_string(x, str) of 
      true  => SOME xs
    | false => case all_except_option(str, xs) of 
                 NONE   => NONE
               | SOME y => SOME (x::y)

fun get_substitutions1 ([], str) = [] 
  | get_substitutions1 (x :: xs, str) =  
    case all_except_option(str, x) of
      NONE   => get_substitutions1(xs, str)
    | SOME y => y @ get_substitutions1(xs, str)
              
fun get_substitutions2 (lst, str) =
  let fun aux(lst, str, acc) =
    case lst of 
      []    => acc
    | y::ys => case all_except_option(str, y) of
                NONE   => aux(ys, str, acc)
              | SOME z => aux(ys, str, z @ acc)
  in 
    aux(lst, str, [])
  end

fun similar_names (lst, full_name) =
  let fun construct_names(lst, full_name, accumulator) = 
        case lst of 
          []    => accumulator
        | x::xs => case full_name of 
                     {first, middle, last} => construct_names(xs, full_name,
                             {first=x, middle=middle, last=last} :: accumulator)
  in
    case full_name of 
      {first, middle, last} => construct_names(first::get_substitutions2(lst, first),
                                               full_name, [])
  end

(* put your solutions for problem 2 here *)
