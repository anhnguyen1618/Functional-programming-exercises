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

(* put your solutions for problem 2 here *)
fun all_except_option(x, xs) =
    let
	fun f [] = []
	  | f (h::t) = if same_string(x, h) then f(t) else h::f(t) 
	val result = f xs
    in
	if result = xs then NONE else SOME result
    end		 

fun get_substitutions1 ([], _) = []
  | get_substitutions1 (h::t, name) = 
    case all_except_option(name, h) of
	NONE => get_substitutions1(t, name)
      | SOME l => l@get_substitutions1(t, name)

fun get_substitutions2 (ls, name) =
    let
	fun helper ([], acc) = acc
	  | helper ((h::t), acc) =
	    case all_except_option(name, h) of
		NONE => helper(t, acc)
	      | SOME l => helper(t, acc@l)
    in
	helper(ls, [])
    end
	
fun similar_names(ls, {first, middle, last}) =
    let
	val same_names = get_substitutions2(ls, first)
	fun f [] = []
	  | f (h::t) = {first = h , middle = middle, last = last}::f(t)
    in
	{first = first, middle = middle, last = last}::f(same_names)
    end

(* This is the second problem *)

fun card_color (suit, _) =
    case suit of
	Diamonds => Red
      | Hearts => Red
      | _ => Black

fun card_value (_, rank) =
    case rank of
	Num(a) => a
      | Ace => 11
      | _ => 10
		 
fun remove_card (cs, c, e) =
    let
	fun f [] = []
	  | f (h::t) = if h = c then t else h::f(t)
	val  result = f cs
    in
	if result = cs
	then raise e
	else result
    end

fun all_same_color [] = true
  | all_same_color (_::[]) = true 
  | all_same_color (h::n::t) = card_color(h) = card_color(n)
			       andalso all_same_color(n::t)

fun sum_cards cs =
    let
	fun f ([], acc) = acc
	  | f (h::t, acc) = f(t, acc + card_value(h))
    in
	f(cs, 0)
    end

fun score (cs, goal) =
    let
	val sum = sum_cards cs
	val pre_score = if sum > goal then 3 * (sum - goal) else goal - sum
    in
	if all_same_color cs then pre_score div 2 else pre_score					    
    end

fun officiate (card_list, moves, goal) =
    let
	fun play_game (card_list, moves, held_cards) =
	    case (card_list, moves) of
		([], _) => held_cards
	      | (_, []) => held_cards
	      | (cl, (Discard c)::t) => play_game(cl, t, remove_card(held_cards, c, IllegalMove))
	      | ((drawed_card:: tl_cards), (Draw)::t) =>
		let
		    val new_held_cards = drawed_card::held_cards
		    val sum_after_draw = sum_cards (new_held_cards)
		in
		    if sum_after_draw > goal
		    then new_held_cards
		    else play_game(tl_cards, t, new_held_cards)
		end
	val final_held_cards = play_game(card_list, moves, [])
    in
	score (final_held_cards, goal)
    end
	
(* Challenge section*)
fun sum_cards_challenge cs =
    let
	fun f ([], acc) = acc
	  | f ((_, Ace)::t, acc) = f(t, acc + 1)
	  | f (h::t, acc) = f(t, acc + card_value(h))
    in
	f(cs, 0)
    end
		
fun score_challenge (cs, goal) =
    let
	fun cal_num_ace ls =
	    let
		fun f ([], acc) = acc
		  | f ((_, Ace):: t, acc) = f(t, acc + 1)
		  | f (_::t, acc) =  f(t, acc)
	    in
		f(ls, 0)
	    end

	fun cal_score sum = if sum > goal then 3 * (sum - goal) else goal - sum

	val normal_sum = sum_cards cs
	val num_ace = cal_num_ace cs									
	fun get_min_score (folded_num, min_score) =
	    if folded_num > num_ace then min_score
	    else
		let
		    val cur_score = cal_score(normal_sum - folded_num * 10)
		    val min = if cur_score < min_score then cur_score else min_score							
		in
		    get_min_score(folded_num + 1, min)
		end
	val pre_score = get_min_score(0, cal_score(normal_sum))
    in
	if all_same_color cs then pre_score div 2 else pre_score
    end
	
fun officiate_challenge (card_list, moves, goal) =
    let
	fun play_game (card_list, moves, held_cards) =
	    case (card_list, moves) of
		([], _) => held_cards
	      | (_, []) => held_cards
	      | (cl, (Discard c)::t) => play_game(cl, t, remove_card(held_cards, c, IllegalMove))
	      | ((drawed_card:: tl_cards), (Draw)::t) =>
		let
		    val new_held_cards = drawed_card::held_cards
		    val sum_after_draw = sum_cards_challenge (new_held_cards)
		in
		    if sum_after_draw > goal
		    then new_held_cards
		    else play_game(tl_cards, t, new_held_cards)
		end
		    
	val final_held_cards = play_game(card_list, moves, [])
    in
	score_challenge (final_held_cards, goal)
    end


(* DISCLAIMER: This function passes the tests but it doesn't work properly for all the cases and the code is pretty ugly.
=> you can have a look if you have time*)
fun careful_player(cards, goal) =
    let
	fun find_removable_card ([], value) = NONE
	  | find_removable_card (h::t, value) =
	    if card_value(h) = value
	    then SOME h
	    else find_removable_card(t, value)

	fun reverse cs =
	    let
		fun f ([], acc) = acc
		  | f (h::t, acc) = f(t, h::acc)
	    in
		f(cs, [])
	    end
			
	fun play_game(cards, moves, held_cards, cur_sum) = 
	    case (cards, cur_sum - goal) of
		(_, 0) => moves
	      |	([], _) => moves
	      | (h::[],_) => if cur_sum + card_value(h) < goal
			     then Draw::moves
			     else
				 let
				     val removed_card = find_removable_card(held_cards, cur_sum + card_value(h) - goal)
				 in
				     case removed_card of
					 NONE => moves
				       | SOME(removed_card) => Draw::(Discard(removed_card)::moves)
				 end

	      | (h::n::t,_) => if cur_sum + 10 < goal
			       then play_game(n::t, Draw::moves, h::held_cards, cur_sum + card_value(h))
			       else if cur_sum + card_value(h) = goal then Draw::moves
			       else
				   let
				       val temp_sum = cur_sum + card_value(h)
				       val overloaded_sum = temp_sum + card_value(n)
				       val is_current_card_takable = temp_sum < goal
				       val removed_card =
					   if is_current_card_takable
					   then
					       find_removable_card(h::held_cards, overloaded_sum - goal)
					   else
					       find_removable_card(held_cards, temp_sum - goal)
				   in				       
				       case removed_card of
					   NONE => if is_current_card_takable then play_game(n::t, Draw::moves, h::held_cards, cur_sum + card_value(h)) else moves
					 | SOME(removed_card) =>
					   if
					       is_current_card_takable
					   then
					       play_game(n::t, Draw::moves, h::held_cards, cur_sum + card_value(h))
					   else
					       play_game(n::t, Draw::Discard(removed_card)::moves, h::held_cards, cur_sum)
				   end
    in
	reverse(play_game(cards, [], [], 0))
    end
	
