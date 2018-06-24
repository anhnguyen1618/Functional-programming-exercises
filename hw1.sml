fun is_older ((year1, month1, day1), (year2, month2, day2)) =
    let
	val [year_gap, month_gap, day_gap] = [year1 - year2, month1 - month2, day1 - day2]
    in
	year_gap < 0
	orelse (year_gap = 0 andalso month_gap < 0)
	orelse (year_gap = 0 andalso month_gap = 0 andalso day_gap < 0)
    end;

fun number_in_month (dates, month) =
    List.foldl (fn ((_, m, _), acc) => acc + (if m = month then 1 else 0)) 0 dates

fun number_in_months (dates, months) =
    List.foldl (fn (x, acc) => number_in_month(dates, x) + acc) 0 months
							    
fun dates_in_month (dates, month) =
    List.filter (fn (_, m, _) => m = month) dates

fun dates_in_months (dates, months) =
    List.foldl (fn (month, acc) => acc@dates_in_month(dates, month)) [] months

exception NotFound
fun get_nth (h::_, 1) = h
  | get_nth (h::t, n) = get_nth(t, n - 1)
  | get_nth (_, _) = raise NotFound

fun date_to_string (y, m, d) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
    end

fun number_before_reaching_sum (sum, num_list) =
    case num_list of
	[] => 0
      | h::t => if sum <= h then 0 else number_before_reaching_sum(sum - h, t) + 1
	
fun what_month date =
    let
	val num_of_days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(date, num_of_days_in_month) + 1
    end


fun month_range (first_day: int, second_day: int) =
    if first_day > second_day then []
    else
	let
	    fun get_months (current_day: int) =
		if current_day = second_day
		then [what_month(current_day)]
		else what_month(current_day)::get_months(current_day +1)
	in
	    get_months(first_day)
	end

fun oldest [] = NONE
  | oldest (h::[]) = SOME h
  | oldest (h::t) =
    let
	val tail_max = oldest t
    in
	case tail_max of
	    NONE => NONE
	  | SOME v => if is_older(h, v) then SOME h else tail_max
    end
	    

(* Challenge problem session*)
	    
fun unique [] = []
  | unique (h::t) =
    let
	fun is_in xs x = List.foldl (fn (cur, acc) => acc andalso cur = x) true xs
	val unique_list = unique t
    in
	if is_in unique_list h
	then unique_list
	else h:: unique_list
    end

fun number_in_months_challenge ([], []) = 0
  | number_in_months_challenge (dates, months) = number_in_months(dates, unique(months))
	
fun dates_in_months_challenge ([], []) = []
  | dates_in_months_challenge (dates, months) = dates_in_months(dates, unique(months))
							       
fun reasonable_date (year, month, day) =
    let
	val is_leap_year = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	val num_of_days_in_month = [31,(if is_leap_year then 29 else 28),31,30,31,30,31,31,30,31,30,31]						       
    in
	not (
	    year < 1
	    orelse (month < 1 orelse month >12)
	    orelse (day < 0 orelse day > get_nth(num_of_days_in_month, month))
	)
    end
    
