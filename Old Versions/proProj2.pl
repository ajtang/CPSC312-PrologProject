:- dynamic(rpath/2).
job(matt,alice).
%setof(Dis,X^closest(ubc,X,Dis),Lst).
%Lst = [0.0, 10.696209894418036].

getem(Loc,Lst) :- setof(Dis,X^closest(Loc,X,Dis),Lst).

driver(matt,[],ubc).
driver(calvin, [], oakridge).
driver(ford,[1],sfu).
driver(adrien,[1],pacificCentre).

drivers([matt,
		 ford,
		 adrien,
		 calvin]).

driver_is_free(X) :-
	driver(X,[],_).

customer(hobbes).
customer(alice).
customer(bob).
customer(bill).
customer(charlie).


is_within(location(_,Y,X)) :-
	X > -123.26,
  X < -122.9
  Y > 49.15,
  Y < 49.325.


pickup(C,Lat,Long,Dest) :-
  (nodes(A),
  	member(Dest, A),
  customer(C),
  is_within(location(_,Lat,Long))
  -> assert(location(C,Lat,Long)),
  insert_edge(C),
  pathfind(C,Dest),
  ; assert(customer(C)),
  writef('Customer ~c does not exist, creating now.', [C]),
  pickup(C,Lat,Long,Dest)).

insert_edge(E) :-
  nodes(A).
  min_pickup(E, A, R),
  location(E, A, B),
  location(R, C, D),
  distance(A, B, C, D, Z),
  assert(edge(E, R, Z)),
  assert(edge(R, E, Z)).

min_pickup(P, Loc) :-
	getLoc(Loc,Lst),
	list_min(Lst,Min),
	location(P,Lat,Lon),
	location(Loc,Lat0,Lon0),
	distance(Lat,Lon,Lat0,Lon0,Dis),
	Min = Dis.

closest(Loc,Loc0,Driver1) :-
	getem(Loc,Lst),
	secondSmallest(Lst, Min),
	driver(Driver1,[],Loc0),
	location(Loc,Lat,Lon),
	location(Loc0,Lat0,Lon0),
	distance(Lat,Lon,Lat0,Lon0,Dis),
	Min = Dis.

list_min([L|Ls], Min) :-
    list_min(Ls, L, Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).

getem(Loc,Lst) :- setof(Dis,X^closest(Loc,X,Dis),Lst).
 
min_pickup(P,[], R, R).
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

delMember(X, [], []) :- !.
delMember(X, [X|Xs], Y) :- !, delMember(X, Xs, Y).
delMember(X, [T|Xs], Y) :- !, delMember(X, Xs, Y2), append([T], Y2, Y).
	
distance(Lat1, Lon1, Lat2, Lon2, Dis):-
    P is 0.017453292519943295,
    A is (0.5 - cos((Lat2 - Lat1) * P) / 2 + cos(Lat1 * P) * cos(Lat2 * P) * (1 - cos((Lon2 - Lon1) * P)) / 2),
    Dis is (12742 * asin(sqrt(A))).
    
pathfind(P,D) :-
	location(P, A, B),
	closest(location(P, A, B), X, Loc),
  driver_path(Loc, P),
  customer_path(P, D),
  retract(location(P, _, _)).
  
% Person - driver/customer is at location Loc.
is_at(Person,Loc).

is_at(matt,ubc).
is_at(bob,sfu).

edge(location(ubc,49.26,-123.25), location(_, Y, X), Z) :-
	Y > 49.241,
  Y < 49.279,
  X < -123.215,

  Some distance calculation.
location(ubc,49.26,-123.25).
location(sfu,49.29,-122.92).
location(pacificCentre,49.28,-123.12).
location(scienceWorld,49.27,-123.10).
location(granvilleIsland,49.27,-123.13).
location(stGeorgeSchool,49.24,-123.19).
location(aquarium,49.30,-123.13).
location(jerichoBeach,49.27,-123.19).
location(mcArthurGlen,49.20,-123.14).
location(mountainViewCemetary,49.24,-123.09).
location(oakridge,49.23,-123.11).
location(langara,49.22,-123.10).
location(metrotownMall,49.22,-122.10).
location(vancouverGeneralHospital,49.26,-123.12).
location(broadwayCinema,49.26,-123.13).
location(museumOfVancouver,49.27,-123.15).
location(yvr,49.16,-123.18).
location(shaughnessyGolfClub,49.24,-123.20).
location(ikeaRichmond,49.19,-123.08).
location(bcit,49.25,-123.00).
location(nikkeiMuseum,49.21,-122.96).
location(sunYatSenGarden,49.28,-123.10).
location(canadaPlace,49.28,-123.11).

% set up our edgy edges
edge(sfu,ubc,23.907683921701715).
edge(sfu,bcit,6.4532921830642564).
edge(langara,bcit,7.532681146178593).
edge(ubc,jerichoBeach,4.388713165610388).
edge(ubc,oakridge,10.330307897153947).
edge(ubc,yvr,8.807029876751669).
edge(oakridge,yvr,7.142485596765709).
edge(yvr,ikeaRichmond,7.532681146179575).
edge(sfu,broadwayCinema,15.305326278782386).
edge(ubc,broadwayCinema,8.658000000000328).
edge(granvilleIsland,broadwayCinema,0.7215000000003692).
edge(granvilleIsland,scienceWorld,2.164500000000082).
edge(scienceWorld,aquarium,3.061065255756318).
edge(sunYatSenGarden,canadaPlace,0.7215000000003692).
edge(broadwayCinema,canadaPlace,2.0407101705043327).
edge(granvilleIsland,canadaPlace,1.6133230457657775).

% list of all places
nodes([ubc,
	   sfu,
	   pacificCentre,
	   scienceWorld,
	   granvilleIsland,
	   stGeorgeSchool,
	   aquarium,
	   jerichoBeach,
	   mcArthurGlen,
	   mountainViewCemetary,
	   oakridge,
	   langara,
	   metrotownMall,
	   vancouverGeneralHospital,
	   broadwayCinema,
	   museumOfVancouver,
	   yvr,
	   shaughnessyGolfClub,
	   ikeaRichmond,
	   bcit,
	   nikkeiMuseum,
	   sunYatSenGarden,
	   canadaPlace]).

connectedif(Loc1,Loc2) :-
 	location(Loc1,X1,Y1),
 	location(Loc2,X2,Y2),
 	dist(Loc1,Loc2,W),
 	W < 0.1.

min_dist([],Min,Min).
min_dist([L|Ls],Min0,Min) :-
	Min1 is min(L,Min0),
	min_dist(Ls,Min1,Min).


% hails a driver who is free or the closest then the driver that 
hail_driver(Loc) :-
 	driver_is_free(X),
 	driver(X,_,Loc1),
 	dist(Loc,Loc1,W),
 	write(X), write(' is your driver, and he is '), write(W), write('km away.').

% 
min_dist([],Min,Min).

min_dist([L|Ls],Min0,Min) :-
	Min1 is min(L,Min0),
	min_dist(Ls,Min1,Min).

dist(Loc1,Loc2,Weight) :-
	location(Loc1,X1,Y1),
	location(Loc2,X2,Y2),
	X is (X2 - X1),
	Y is (Y2 - Y1),
	Weight is sqrt((X*72.15)^2+(Y*72.15)^2).



% --------------------------------------------------------------------------------------------------------------------------------------------------------

% assert creates new facts
% I tried to make a parser that takes the coordinate system and makes it into dist facts, which would serce as edges.
% you input your location, and it would make edges from your location to all nodes.

% makeGraph(Loc) :-
% 	driver(X,_,Loc1),
% 	asserta(dist(Loc,Loc1,W)),
% 	write('New Driver:'), write(X), write(' is '), write(W), write('km away.').

% --------------------------------------------------------------------------------------------------------------------------------------------------------

% for now I just made a bunch of dist1 facts manually to test this, supposing you inputted aquarium as your location
% Ive copied over an implementation of dijkstras and modified it a bit, not really, but whatever
% run hail_driver2(aquarium,uber) to get the shortest path that has an uber on it.

dist1(aquarium,ubc,9.126333327246234).
dist1(aquarium,sfu,15.168668844034606).
dist1(aquarium,pacificCentre,1.613323045765548).
dist1(aquarium,langara,6.1644987022465125).
dist1(aquarium,oakridge,5.252599285115835).
dist1(aquarium,10.049332141987964).

% these facts are always in place, they mark where the ubers are

dist1(ubc,uber,0).
dist1(sfu,uber,0).
dist1(pacificCentre,uber,0).

% the weights are always 0, assuming there are no passengers, weights could be changed to 100 or something to give nodes priority


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

customer_path(From, To) :-
	traverse(From),                   			% Find all distances
	rpath([To|RPath], Dist)->         			% If the target was reached
	  reverse([To|RPath], Path),      			% Report the path and distance
	  Distance is Dist,
	  writef('The route from pickup is %w with distance %w\n km.',
	       [Path, Distance]);
	writef('There is no route from %w to %w\n because you have no driver.', [From, To]).


driver_path(From, To) :-
	traverse(From),                   			% Find all distances
	rpath([To|RPath], Dist)->         			% If the target was reached
	  reverse([To|RPath], Path),      			% Report the path and distance
	  Distance is Dist,
	  writef('The Driver is enroute through %w and is distance %w\n km away.', [Path, Distance]);
	writef('There is no driver enroute from %w to %w\n.', [From, To]).



my_max([], R, R). %end
my_max([X|Xs], WK, R):- X >  WK, my_max(Xs, X, R). %WK is Carry about
my_max([X|Xs], WK, R):- X =< WK, my_max(Xs, WK, R).
my_max([X|Xs], R):- my_max(Xs, X, R). %start


