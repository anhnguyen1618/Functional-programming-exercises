datatype node =
	 Leaf
	 | Node of int * node * node

fun calsum x =
    case x of
	Leaf => 0
      | Node(v,l,r) => v + calsum(l) + calsum(r)

val sum = calsum (
	Node(
	    1,
	    Node(2, Node(4, Leaf, Leaf), Node(6, Leaf, Leaf)),
	    Node(3, Node(5, Leaf, Leaf), Node(7, Leaf, Leaf))
	)
    );

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp * exp
	     | Multiply of exp * exp

fun get_constants e =
    case e of
	Constant i => [i]
      | Negate e => get_constants e
      | Add(a,b) => get_constants(a)@get_constants(b)
      | Multiply(x,y) => get_constants(x)@get_constants(y)
	
fun largest_constant e =
    let
	fun get_largest(a,b) =
	    let
		val x = largest_constant(a)
		val y = largest_constant(b)
	    in
		if x > y then x else y
	    end
    in
	case e of
	    Constant i => i
	  | Negate e => largest_constant e
	  | Add(a,b) => get_largest(a,b)	
	  | Multiply(x,y) => get_largest(x,y)
    end

val a = get_constants(Add (Constant 19, Negate (Constant 4)))
val b = largest_constant(Add (Constant 19, Negate (Constant 4)))

datatype x = NONE
       | SUM of int*int*int;

fun find x =
    case x of
	NONE => 0
      | SUM(x) => (#1 x)
			       
val a = find (SUM(1,2,3))


fun zip x =
    case x of
	([],[],[]) => []
      | (a::b, c::d, e::f) => (a, c, e)::zip(b, d, f)
      | _ => []

fun unzip x =
    case x of
	[] => ([], [], [])
      | (a,b,c)::d => let val (x, y, z) = unzip d
		      in
			  (a::x, b::y, c::z)
		      end

fun nondecreasing xs =
    case xs of
	[] => true
      | x::[] => true
      | x::y::z => x <= y andalso nondecreasing(y::z)

	
