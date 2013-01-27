(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
 
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


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove


(* put your solutions for problem 2 here *)

fun card_color card = 
  case card of 
    (Clubs, _)   => Black 
  | (Spades, _)  => Black 
  | (Diamonds,_) => Red
  | (Hearts,_)   => Red

fun card_value card =
  case card of 
    (_, Ace)   => 11 
  | (_, Num n) => n
  | (_, _)     => 10

fun remove_card ([], c, e : exn) = raise e
  | remove_card (card::cs, c, e : exn) = 
    case card=c of 
      true  => cs
    | false => card::remove_card(cs,c,e)
   
fun all_same_color([]) = true
  | all_same_color([card]) = true 
  | all_same_color(card::rest) =
    case rest of
      crd::tail => case card_color(card)=card_color(crd) of
                     true  => all_same_color(rest)
                   | false => false

fun sum_cards cards = 
  let fun aux([], acc) = acc
        | aux(card::rest, acc) = 
          aux(rest, acc + card_value card)
  in
    aux(cards, 0)
  end

fun score(cards, goal) = 
  let val held_cards_sum = sum_cards(cards)
      val preliminary_score = if held_cards_sum > goal 
                              then 3*(held_cards_sum-goal)
                              else goal-held_cards_sum
  in 
    if all_same_color(cards) then preliminary_score div 2
    else preliminary_score
  end

fun officiate(cards, moves, goal) = 
  let fun loop(game_state) = 
          case game_state of 
            (_,[],goal,held) => score(held,goal)
          | ([], Draw::Moves, goal, held) => score(held,goal)
          | (card::cards, move::moves, goal, held) => 
              case move of 
                Discard crd => loop(cards,moves,goal,(remove_card(held,crd,IllegalMove)))
              | Draw => if (card_value(card)+sum_cards(held)) > goal
                        then score(card::held,goal)
                        else loop(cards,moves,goal,card::held)
  in
    loop(cards,moves,goal,[])
  end
