:- dynamic(rpath/2).
:- dynamic(nodes/2).
:- dynamic(factorial/2).

% driver(A,[B],C) with A being name, 
% B being [] or [1], dictating if the driver is free (free if []),
% C is current location.
driver(matt,[],ubc).
driver(calvin, [], oakridge).
driver(ford,[1],sfu).
driver(packo,[],sfu).
driver(rob,[],nikkeiMuseum).
driver(adrien,[1],pacificCentre).
driver(james, [], canadaPlace).
driver(eve,[], metrotownMall).
driver(allen,[], kitsBeach).

driver_is_free(X) :-
	driver(X,[],_).

% defined customers part of the KB
customer(hobbes).
customer(alice).
customer(bob).
customer(bill).
customer(charlie).

my_min([], R, R). %end
my_min([X|Xs], WK, R):- X >  WK, my_min(Xs, WK, R). %WK is Carry about
my_min([X|Xs], WK, R):- X =< WK, my_min(Xs, X, R).
my_min([X|Xs], R):- my_min(Xs, X, R). %start

% Request pickup and path for customer
main :- 
	write('Hello and welcome. Please enter your name.' ), nl, read(X), readCust(X).

% check if customer name exists, if not, then add to KB
readCust(C) :-
	first_char_uppercase(C,Cs),
	(customer(C)
	 -> 
	 writef('Welcome back %w! Please enter your current location.', [Cs]), nl,
	 writef('Latitude '), read(Y),
	 writef('Longtitude '), read(X), nl,
	 checkLoc(C,Y,X)
	 ; writef('Customer %w does not exist, creating now.', [Cs]) , nl,
	 writef('Welcome %w! Please enter your current location.', [Cs]), nl,
	 writef('Latitude '), read(Y),
	 writef('Longtitude '), read(X), nl,
	 checkLoc(C,Y,X)).

% check if Customer location is within bounds
checkLoc(C,Y,X) :-
	(is_bound(Y,X)
	 -> writef('Please enter your destination.'), nl, read(A), checkDest(C,Y,X,A)
	 ; writef('Error. Current location out of bounds.'), nl,
	 writef('Please enter a new Latitude '), read(P), 
	 writef('Please enter a new Longtitude '), read(Q),nl, checkLoc(C,P,Q)).

% check if destination is valid, then run pathfind
checkDest(C,Y,X,Dest) :-
	(place(Dest)
		-> first_char_uppercase(C,Cs),
		writef('Thank you %w. Contacting available drivers.', [Cs]), nl,
		writef('...'), nl, sleep(2),
		assert(location(C,Y,X)),
   		insert_edge(C),
   		pathfind(C,Dest)
   		; writef('Error. Please enter a valid destination '), read(A), nl, checkDest(C,Y,X,A)
		).
		

% helper: check if customer is within bounds
is_bound(Y,X) :-
  X > -123.26,
  X < -122.71,
  Y > 49.15,
  Y < 49.325.

% inserting customer position to edge to its nearest location.
insert_edge(E) :-
  findall(X, place(X), Lst),
  min_pickup(E, Lst, R),
  location(E, A, B),
  location(R, C, D),
  distance(A, B, C, D, Z),
  assert(edge(E, R, Z)),
  assert(edge(R, E, Z)).

% intiate optimal path finding  
pathfind(P,D) :-
	location(P, _, _),
	findall(X, driver(X, [], _), Lst),
	closest_driver(P, Lst, Dr),
    driver_path(X, P, Dr),
    customer_path(P, D),
    retract(location(P, _, _)).

% check for closet location to customer position
min_pickup(_,[], R, R).
min_pickup(P, [X|Xs], W, R) :-
	location(P, Lt, Lng),
	location(X, A, B),
    location(W, C, D),
	distance(Lt, Lng, A, B, E),
    distance(Lt, Lng, C, D, F),
    E > F,
   min_pickup(P, Xs, W, R).
min_pickup(P, [X|Xs], W, R) :-
	location(P, Lt, Lng),
	location(X, A, B),
  location(W, C, D),
	distance(Lt, Lng, A, B, E),
  distance(Lt, Lng, C, D, F),
  E =< F,
  min_pickup(P, Xs, X, R). 
min_pickup(P,Lst,R) :-
	location(X,_,_),
	delMember(P, Lst, [X|Xs]),
    min_pickup(P, Xs, X, R).

% delete all 'X' elements in a given list
delMember(_, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).

% check for closest driver to customer position
closest_driver(_,[], R, R).
closest_driver(P, [X|Xs], W, R) :-
	driver(X,[],Loc1),
	driver(W,[],Loc2),
	location(P, Lt, Lng),
	location(Loc1, A, B),
    location(Loc2, C, D),
	distance(Lt, Lng, A, B, E),
    distance(Lt, Lng, C, D, F),
    E > F,
    closest_driver(P, Xs, W, R).
closest_driver(P, [X|Xs], W, R) :-
	driver(X,[],Loc1),
	driver(W,[],Loc2),
	location(P, Lt, Lng),
	location(Loc1, A, B),
    location(Loc2, C, D),
	distance(Lt, Lng, A, B, E),
    distance(Lt, Lng, C, D, F),
    E =< F,
    closest_driver(P, Xs, X, R). 
    closest_driver(P,[X|Xs],R) :-
    closest_driver(P, Xs, X, R).
  
% helper to check to locations distance
calc(A,X,D) :-
	location(A,B,C),
	location(X,Y,Z),
	distance(B,C,Y,Z,D).

% Using lat long, check distance in km
distance(Lat1, Lon1, Lat2, Lon2, Dis):-
    P is 0.017453292519943295,
    A is (0.5 - cos((Lat2 - Lat1) * P) / 2 + cos(Lat1 * P) * cos(Lat2 * P) * (1 - cos((Lon2 - Lon1) * P)) / 2),
    Dis is (12742 * asin(sqrt(A))).
% Person - driver/customer is at location Loc.
is_at(matt,ubc).
is_at(bob,sfu).

% Defined locations with coordinates
location(ubc,49.26, -123.25).
location(sfu,49.29, -122.92).
location(pacificCentre,49.28, -123.12).
location(scienceWorld,49.27, -123.10).
location(granvilleIsland,49.27, -123.13).
location(stGeorgeSchool,49.24, -123.19).
location(jerichoBeach,49.27, -123.19).
location(kitsBeach, 49.27, -123.15).
location(oakridge,49.23, -123.11).
location(langara,49.22, -123.10).
location(metrotownMall,49.22, -122.99).
location(vancouverGeneralHospital,49.26, -123.12).
location(broadwayCinema,49.26, -123.13).
location(museumOfVancouver,49.27, -123.15).
location(yvr, 49.16, -123.18).
location(shaughnessyGolfClub,49.24, -123.20).
location(ikeaRichmond,49.19, -123.08).
location(bcit,49.25, -123.00).
location(nikkeiMuseum,49.21, -122.96).
location(sunYatSenGarden,49.28, -123.10).
location(canadaPlace,49.28, -123.11).

% set up our edgy edges
edge(ubc, shaughnessyGolfClub,4.256367128365631).
edge(shaughnessyGolfClub, ubc,4.256367128365631).
edge(ubc, jerichoBeach, 4.49344583039421).
edge(jerichoBeach, ubc, 4.49344583039421).
edge(jerichoBeach, kitsBeach, 2.9021665251911246).
edge(kitsBeach, jerichoBeach, 2.9021665251911246).
edge(ubc, kitsBeach, 7.340855558144903).
edge(kitsBeach, ubc, 7.340855558144903).
edge(shaughnessyGolfClub, langara, 7.5942183566949115).
edge(langara,shaughnessyGolfClub, 7.5942183566949115).
edge(langara, oakridge, 1.3280821756038954).
edge(oakridge, langara, 1.3280821756038954).
edge(oakridge, kitsBeach, 5.311524910832483).
edge(kitsBeach, oakridge, 5.311524910832483).
edge(kitsBeach, granvilleIsland, 1.451083275662867).
edge(granvilleIsland, kitsBeach, 1.451083275662867).
edge(granvilleIsland, canadaPlace,1.8280172340102139).
edge(canadaPlace,granvilleIsland, 1.8280172340102139).
edge(granvilleIsland, pacificCentre, 1.3276803558523267).
edge(pacificCentre, granvilleIsland, 1.3276803558523267).
edge(canadaPlace, sunYatSenGarden,0.7253945607478096).
edge(sunYatSenGarden,canadaPlace,0.7253945607478096).
edge(sunYatSenGarden, oakridge, 5.606916255799196).
edge(oakridge, sunYatSenGarden, 5.606916255799196).
edge(vancouverGeneralHospital, granvilleIsland, 1.3277607211353992).
edge(granvilleIsland, vancouverGeneralHospital, 1.3277607211353992).
edge(vanDusen, vancouverGeneralHospital, 2.3393507991395452).
edge(vancouverGeneralHospital, vanDusen, 2.3393507991395452).
edge(granvilleIsland,scienceWorld,2.164500000000082).
edge(scienceWorld,granvilleIsland, 2.164500000000082).
edge(metrotownMall,oakridge, 8.785092701539194).
edge(oakridge, metrotownMall, 8.785092701539194).
edge(langara,oakridge,1.3280821756038954).
edge(oakridge, langara, 1.3280821756038954).
edge(metrotownMall, bcit, 3.4139475879134227).
edge(bcit, metrotownMall, 3.4139475879134227).
edge(bcit, vancouverGeneralHospital,8.7798432934765).
edge(vancouverGeneralHospital,bcit, 8.7798432934765).
edge(metrotownMall, nikkeiMuseum, 2.4463630474313733).
edge(nikkeiMuseum, metrotownMall, 2.4463630474313733).
edge(nikkeiMuseum, bcit, 5.31216781716127).
edge(bcit, nikkeiMuseum, 5.31216781716127).
edge(sfu,ubc,23.907683921701715).
edge(ubc,sfu,23.907683921701715).
edge(sfu,bcit,6.4532921830642564).
edge(bcit, sfu, 6.4532921830642564).
edge(sfu, nikkeiMuseum, 9.35740284019287).
edge(nikkeiMuseum, sfu, 9.35740284019287).
edge(langara,bcit,7.532681146178593).
edge(bcit,langara,7.532681146178593).
edge(ubc,oakridge,10.330307897153947).
edge(oakridge, ubc, 10.330307897153947).
edge(shaughnessyGolfClub, yvr, 9.013501669001993).
edge(yvr, shaughnessyGolfClub, 9.013501669001993).
edge(oakridge,yvr,7.142485596765709).
edge(yvr, oakridge,7.142485596765709).
edge(yvr,ikeaRichmond,7.532681146179575).
edge(ikeaRichmond, yvr,7.532681146179575).
edge(oakridge, ikeaRichmond, 4.952990958279662).
edge(ikeaRichmond, oakridge, 4.952990958279662).
edge(sfu,broadwayCinema,15.305326278782386).
edge(ubc,broadwayCinema,8.658000000000328).
edge(granvilleIsland,broadwayCinema,0.7215000000003692).
edge(granvilleIsland,scienceWorld,2.164500000000082).
edge(scienceWorld,aquarium,3.061065255756318).
edge(sunYatSenGarden,canadaPlace,0.7215000000003692).
edge(broadwayCinema,canadaPlace,2.0407101705043327).
edge(granvilleIsland,canadaPlace,1.6133230457657775).

% defined places with no coordinates.
place(ubc).
place(sfu).
place(pacificCentre).
place(scienceWorld).
place(granvilleIsland).
place(stGeorgeSchool).
place(aquarium).
place(jerichoBeach).
place(mcArthurGlen).
place(mountainViewCemetary).
place(oakridge).
place(langara).
place(metrotownMall).
place(vancouverGeneralHospital).
place(broadwayCinema).
place(museumOfVancouver).
place(yvr).
place(shaughnessyGolfClub).
place(ikeaRichmond).
place(bcit).
place(nikkeiMuseum).
place(sunYatSenGarden).
place(canadaPlace).


% Dijkstra Algorithm Section
path(From, To, Dist) :- edge(To, From, Dist).
path(From, To, Dist) :- edge(From, To, Dist).

shorterPath([H|Path], Dist) :-
    (rpath([H|_], D) -> Dist < D -> retract(rpath([H|_], _)); true),
    assertz(rpath([H|Path], Dist)).

traverse(From, Path, Dist) :-
    path(From, T, D),
    \+ memberchk(T, Path),
    shorterPath([T,From|Path], Dist+D),
    traverse(T, [From|Path], Dist+D).

traverse(From) :-
    retractall(rpath(_, _)),
    traverse(From, [], 0).

traverse(_).

% Optimal path from customer to goal 
customer_path(From, To) :-
	traverse(From),                   			% Find all distances
	rpath([To|RPath], Dist)->         			% If the target was reached
	  reverse([To|RPath], Path),      			% Report the path and distance
	  Distance is Dist,
	  format('The optimal route from pickup is ~w with a travelling distance of ~2f km.\n',
	       [Path, Distance]);
	writef('There is no route from %w to %w\n because you have no driver.\n', [From, To]).

% Optimal path from driver to customer
driver_path(From, To, Dr) :-
	traverse(From),                   			% Find all distances
	rpath([To|RPath], Dist)->         			% If the target was reached
	  reverse([To|RPath], Path),      			% Report the path and distance
	  Distance is Dist,
	  first_char_uppercase(Dr, D),
	  format('Your driver, ~w, is enroute through ~w and is ~2f km away.\n', [D, Path, Distance]);
	writef('There is no driver enroute from %w to %w.\n', [From, To]).

% Random max function
my_max([], R, R). %end
my_max([X|Xs], WK, R):- X >  WK, my_max(Xs, X, R). %WK is Carry about
my_max([X|Xs], WK, R):- X =< WK, my_max(Xs, WK, R).
my_max([X|Xs], R):- my_max(Xs, X, R). %start

% Change first character to uppercase
first_char_uppercase(WordLC, WordUC) :-
    atom_chars(WordLC, [FirstChLow|LWordLC]),
    atom_chars(FirstLow, [FirstChLow]),
    lwrtoupr(FirstLow, FirstUpp),
    atom_chars(FirstUpp, [FirstChUpp]),
    atom_chars(WordUC, [FirstChUpp|LWordLC]).
lwrtoupr(L ,U) :- upcase_atom(L, U).
