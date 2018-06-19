(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

(* 1 *)
fun only_capitals xs = List.filter (fn x => (Char.isUpper o String.sub) (x, 0) ) xs

(* 2 *)
fun longest_string1 xs = List.foldl (fn (c, a) => if String.size c > String.size a then c else a) "" xs

(* 3 *)
fun longest_string2 xs = List.foldl (fn (c, a) => if String.size c >= String.size a then c else a) "" xs

(* 4 *)				    
fun longest_string_helper f = List.foldl (fn (c, acc) => if f (String.size c, String.size acc) then c else acc) ""

val longest_string3 = longest_string_helper op>
					    
val longest_string4 = longest_string_helper op>=
					    					 (* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer _ [] = raise NoAnswer
  | first_answer f (h::tl) =
    case f h of
	NONE => first_answer f tl
      | SOME x => x

(* 8 *)
fun all_answers f =
    let
	fun g(c,acc) =
	    case (f c, acc) of
		(NONE, _) => NONE
	      | (SOME x, SOME a) => SOME (a@x)
	      | _ => NONE
    in
	List.foldl g (SOME [])
    end

(* 9 *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn _ => 1) String.size

fun count_some_var (s, p) = g (fn _ => 0) (fn x => if x = s then 1 else 0) p

(* 10 *)
fun check_pat p =
    let
	fun extract_variable (p, acc) =
	    case p of
		Wildcard          => acc
	      | Variable x        => x::acc
	      | TupleP ps         => List.foldl (extract_variable) acc ps
	      | ConstructorP(_,p) => extract_variable (p, acc)
	      | _                 => acc

	fun check_unique [] = true
	  | check_unique (h::tl) = not (List.exists (fn s => s = h) tl) andalso check_unique tl
	    	   
    in
	(check_unique o extract_variable) (p, [])
    end

(* 11 *)
fun match (v, p) =
    case (p, v) of
	(Wildcard, _) => SOME []
      | (Variable s, v) => SOME [(s,v)]
      | (UnitP, Unit) => SOME []
      | (ConstP y, Const x) => if x = y then SOME [] else NONE
      | (TupleP ps, Tuple vs) => all_answers match (ListPair.zip (Vs, ps))
					     handle UnequalLengths => NONE
      | (ConstructorP(s1,p), Constructor (s2,v)) => if s1 = s2
						    then match (v, p)
						    else NONE
      | _ => NONE

(* 12 *)
fun first_match v ps =
    SOME (first_answer (fn p => match(v, p)) ps)
    handle
    NoAnswer => NONE
