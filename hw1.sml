fun is_older(first_date : (int * int * int), second_date: (int * int * int)) =
    let
	val year_gap = (#1 first_date) - (#1 second_date)
	val month_gap = (#2 first_date) - (#2 second_date)
	val day_gap = (#3 first_date) - (#3 second_date)
    in
	year_gap < 0
	orelse (year_gap = 0 andalso month_gap < 0)
	orelse (year_gap = 0 andalso month_gap = 0 andalso day_gap < 0)
    end;

fun number_in_month(dates: (int * int * int) list, month: int) =
    let
	fun get_number_in_month(dates: (int * int * int) list) =
	    if null dates then 0
	    else
		let
		    val cur_date = hd dates
		    val cur_month = (#2 cur_date)
		    val temp_number = get_number_in_month(tl dates)
		in
		    if cur_month = month then 1 + temp_number
		    else temp_number
		end
    in
	get_number_in_month(dates)
    end;

fun number_in_months(dates: (int * int * int) list, months: int list) =
    if null months orelse null dates then 0
    else
	number_in_month(dates, (hd months)) + number_in_months(dates, tl months)    

fun dates_in_month(dates: (int * int * int) list, month: int) =
    let
	fun get_dates_in_month(dates: (int * int * int) list) =
	    if null dates then []
	    else
		let
		    val cur_date = hd dates
		    val cur_month = (#2 cur_date)
		    val temp_dates = get_dates_in_month(tl dates)
		in
		    if cur_month = month then cur_date::temp_dates
		    else temp_dates
		end
    in
	get_dates_in_month(dates)
    end;

fun dates_in_months(dates: (int * int * int) list, months: int list) =
    if null months orelse null dates then []
    else
	dates_in_month(dates, (hd months))@dates_in_months(dates, tl months)    

fun get_nth(string_list: string list, n: int) =
    if n = 1
    then hd string_list
    else get_nth(tl string_list, n - 1)

fun date_to_string(dates: (int * int * int)) =
    let
	val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
	get_nth(months, (#2 dates)) ^ " " ^ Int.toString(#3 dates) ^ ", " ^ Int.toString(#1 dates)
    end

fun number_before_reaching_sum(sum: int, num_list: int list) =
    if sum <= hd num_list
    then 1
    else 1 + number_before_reaching_sum(sum, num_list)
	
fun what_month(date: int) =
    let
	val num_of_days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
    in
	number_before_reaching_sum(date, num_of_days_in_month) + 1
    end


fun month_range(first_day: int, second_day: int) =
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

fun oldest(dates: (int * int * int) list) =
    if null dates then NONE
    else
	let
	    fun find_oldest(dates: (int * int * int) list) =
		if null (tl dates) then hd dates
		else
		    let
			val cur_date = hd dates
			val tempValue = find_oldest(tl dates)
		    in
			if is_older(cur_date, tempValue)
			then cur_date
			else tempValue
		    end
	in
	    SOME(find_oldest(dates))
	end	    
	    

(* Challenge problem session*)
	    
fun unique(item_list: int list) =
    if null item_list then []
    else
	let
	    fun is_in(item_list: int list, item: int) = not (null item_list) andalso ((hd item_list = item) orelse is_in(tl item_list, item));
	    val unique_list = unique(tl item_list)
	in
	    if is_in(unique_list, hd item_list)
	    then unique_list
	    else (hd item_list):: unique_list
 	end

fun number_in_months_challenge(dates: (int * int * int) list, months: int list) =
    if null months orelse null dates then 0
    else
	number_in_months(dates, unique(months))
	
fun dates_in_months_challenge(dates: (int * int * int) list, months: int list) =
    if null months orelse null dates then []
    else
	dates_in_months(dates, unique(months))
    
fun reasonable_date(date: (int * int * int)) =
    let
	val year = #1 date
	val month = #2 date
	val day = #3 date
	val is_leap_year = year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
	val num_of_days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]						       
	fun get_int_nth(int_list: int list, n: int) =
	    let
		fun get_real_nth(int_list: int list, cur: int) =
		    if cur = n
		    then hd int_list
		    else get_real_nth(tl int_list, cur + 1)
	    in
		get_real_nth(int_list, 1)
	    end

    in
	if year < 1 orelse (month < 1 orelse month >12) orelse (day < 0 orelse day > 31)
	then false
	else
	    if month = 2 andalso is_leap_year
	    then day <= 29
	    else day <= get_int_nth(num_of_days_in_month, month)
    end
    
