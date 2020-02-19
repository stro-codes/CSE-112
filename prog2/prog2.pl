% CSE 112 - Program 2 By Strother Woog

/* initiate search for a viable complete trip */
fly(X, Y) :- 
	X \= Y,
	( (flight(X, Y, T)) ->
		calc_arrive(X, Y, T, Ta),
		airport(X, N1, _, _),
		airport(Y, N2, _, _),
		print_trip(depart, X, N1, T),
		print_trip(arrive, Y, N2, Ta)
	;
		flyHelp(X, Y, [X], A, B, time(0,0)),
		reverse(A, P),
		print_trips(B, P)
	).

/* make a list of viable airports for a complete trip */
flyHelp(X, Y, P, [Y|P], [T, Ta], Parrival) :-
	flight(X, Y, T),
	check_arrive(T, Parrival),
	calc_arrive(X, Y, T, Ta).
flyHelp(X, Y, V, P, [T, Ta| Times], Parrival) :- 
	flight(X, C, T),
	C \= Y,
	not(member(C, V)),
	check_arrive(T, Parrival),
	calc_arrive(X, C, T, Ta),
	flyHelp(C, Y, [C|V], P, Times, Ta).

/* 	checks if the previous arrival time + layover is less than next departure time */
check_arrive(time(Hour, Minutes), time(Phour, Pminutes)) :-
	( (Pminutes + 30) < 60 ->
		Apminutes is Pminutes + 30,
		Chour is 0
	;
		Apminutes is Pminutes - 30,
		Chour is 1
	),
	Aphour is Phour + Chour,
	Aphour =< Hour, 
	( (Aphour = Hour) -> 
		Apminutes < Minutes
	;
		Apminutes < (Minutes + 60)
	).
	
/* calculate arrival times */
calc_arrive(A1, A2, time(Hour, Minute), time(Ahour, Aminute)) :-
	airport(A1, _, X1, Y1),
	airport(A2, _, X2, Y2),
	to_radians(Y1, Y2, X1, X2, Lon1, Lon2, Lat1, Lat2),
	haversine(Lon1, Lon2, Lat1, Lat2, D),
	Ddivmph is D / 500, 
	Dfloor is floor(Ddivmph), 
	Dmin is Ddivmph - Dfloor, 
	time_from_departure(Dfloor, Dmin, Hour, Minute, Ahour, Aminute).

/* add total flight time to departure time */
time_from_departure(Dfloor, Dmin, Dhour, Dminute, Ahour, Aminute) :-
	Cminute is round(Dmin * 60),
	( (Cminute + Dminute) < 60 -> 
		Aminute is Cminute + Dminute,
		Chour is 0 
	; 
		Aminute is Cminute + Dminute - 60,
		Chour is 1
	),
	( (Dfloor + Dhour + Chour) < 25 ->
		Ahour is Dfloor + Dhour + Chour
	;
		Ahour is Dfloor + Dhour + Chour - 24
	).
	
/* haversine formula */
haversine(Lon1, Lon2, Lat1, Lat2, D) :-
	Dlon is abs(Lon2 - Lon1),
	Dlat is abs(Lat2 - Lat1),
	A1 is sin(Dlat / 2)**2,
	A2 is cos(Lat1),
	A3 is cos(Lat2),
	A4 is sin(Dlon / 2)**2,
	A is A1 + A2 * A3 * A4,
	C1 is sqrt(A),
	C2 is sqrt(1 - A),
	C is 2 * atan2(C1, C2),
	R is 3956,
	D is R * C.

/* convert degrees & minutes to deicmal degree to radians */
to_radians(degmin(Lon1deg, Lon1min), degmin(Lon2deg, Lon2min), degmin(Lat1deg, Lat1min), degmin(Lat2deg, Lat2min), Lon1r, Lon2r, Lat1r, Lat2r) :-
	Lon1dd is Lon1deg + (Lon1min/60),
	Lon2dd is Lon2deg + (Lon2min/60),
	Lat1dd is Lat1deg + (Lat1min/60),
	Lat2dd is Lat2deg + (Lat2min/60),
	Lon1r is Lon1dd * pi / 180,
	Lon2r is Lon2dd * pi / 180,
	Lat1r is Lat1dd * pi / 180,
	Lat2r is Lat2dd * pi / 180 .
	
/* print out complete list of trip departures & arrivals */
print_trips([T, Ta| []], [X, Y | []]) :-
	airport(X, N1, _, _),
	airport(Y, N2, _, _),
	print_trip(depart, X, N1, T),
	print_trip(arrive, Y, N2, Ta).
print_trips([T, Ta| Rtime], [X, Y| Rdest]) :-
	airport(X, S1, _, _),
	airport(Y, S2, _, _),
	print_trip(depart, X, S1, T),
	print_trip(arrive, Y, S2, Ta),
	print_trips(Rtime, [Y|Rdest]).

/* trip print function */
print_trip( Action, Code, Name, time( Hour, Minute)) :-
	upcase_atom(Code, Upper_code),
	format("~6s  ~3s  ~s~26|  ~`0t~d~30|:~`0t~d~33|", [Action, Upper_code, Name, Hour, Minute]),
	nl.

/* main */ 
main :-
	read(X),
	read(Y),
	fly(X, Y).
	
/* provided test for print_trip */
test :-
   print_trip( depart, nyc, 'New York City', time( 9, 3)),
   print_trip( arrive, lax, 'Los Angeles', time( 14, 22)).

% Database
   
/* Latitude, Longitude in (Degree, Minutes) */
airport( atl, 'Atlanta         ', degmin(  33,39 ), degmin(  84,25 ) ).
airport( bos, 'Boston-Logan    ', degmin(  42,22 ), degmin(  71, 2 ) ).
airport( chi, 'Chicago         ', degmin(  42, 0 ), degmin(  87,53 ) ).
airport( den, 'Denver-Stapleton', degmin(  39,45 ), degmin( 104,52 ) ).
airport( dfw, 'Dallas-Ft.Worth ', degmin(  32,54 ), degmin(  97, 2 ) ).
airport( lax, 'Los Angeles     ', degmin(  33,57 ), degmin( 118,24 ) ).
airport( mia, 'Miami           ', degmin(  25,49 ), degmin(  80,17 ) ).
airport( nyc, 'New York City   ', degmin(  40,46 ), degmin(  73,59 ) ).
airport( sea, 'Seattle-Tacoma  ', degmin(  47,27 ), degmin( 122,17 ) ).
airport( sfo, 'San Francisco   ', degmin(  37,37 ), degmin( 122,23 ) ).
airport( sjc, 'San Jose        ', degmin(  37,22 ), degmin( 121,56 ) ).

/* Flights */
flight( bos, nyc, time( 7,30 ) ).
flight( dfw, den, time( 8, 0 ) ).
flight( atl, lax, time( 8,30 ) ).
flight( chi, den, time( 8,45 ) ).
flight( mia, atl, time( 9, 0 ) ).
flight( sfo, lax, time( 9, 0 ) ).
flight( sea, den, time( 10, 0 ) ).
flight( nyc, chi, time( 11, 0 ) ).
flight( sea, lax, time( 11, 0 ) ).
flight( den, dfw, time( 11,15 ) ).
flight( sjc, lax, time( 11,15 ) ).
flight( atl, lax, time( 11,30 ) ).
flight( atl, mia, time( 11,30 ) ).
flight( chi, nyc, time( 12, 0 ) ).
flight( lax, atl, time( 12, 0 ) ).
flight( lax, sfo, time( 12, 0 ) ).
flight( lax, sjc, time( 12, 15 ) ).
flight( nyc, bos, time( 12,15 ) ).
flight( bos, nyc, time( 12,30 ) ).
flight( den, chi, time( 12,30 ) ).
flight( dfw, den, time( 12,30 ) ).
flight( mia, atl, time( 13, 0 ) ).
flight( sjc, lax, time( 13,15 ) ).
flight( lax, sea, time( 13,30 ) ).
flight( chi, den, time( 14, 0 ) ).
flight( lax, nyc, time( 14, 0 ) ).
flight( sfo, lax, time( 14, 0 ) ).
flight( atl, lax, time( 14,30 ) ).
flight( lax, atl, time( 15, 0 ) ).
flight( nyc, chi, time( 15, 0 ) ).
flight( nyc, lax, time( 15, 0 ) ).
flight( den, dfw, time( 15,15 ) ).
flight( lax, sjc, time( 15,30 ) ).
flight( chi, nyc, time( 18, 0 ) ).
flight( lax, atl, time( 18, 0 ) ).
flight( lax, sfo, time( 18, 0 ) ).
flight( nyc, bos, time( 18, 0 ) ).
flight( sfo, lax, time( 18, 0 ) ).
flight( sjc, lax, time( 18,15 ) ).
flight( atl, mia, time( 18,30 ) ).
flight( den, chi, time( 18,30 ) ).
flight( lax, sjc, time( 19,30 ) ).
flight( lax, sfo, time( 20, 0 ) ).
flight( lax, sea, time( 22,30 ) ).
