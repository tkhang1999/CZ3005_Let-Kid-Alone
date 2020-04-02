:- dynamic done/1.
:- dynamic not_done/1.
:- dynamic curr/1.
:- dynamic asked/1.

ask(0):- 
		print("Did you "), activity(L), query_main_options(L).

query_main_options(L):- 
		member(X, L), \+asked(X), assert(asked(X)), print(X), print(' today? y/n/q: '), read(Done), (Done==q -> abort;Done==y -> assert(done(X)), assert(curr(X)), ask_follow_up(X);assert(not_done(X)), ask(0)).

ask_follow_up(Y):-
		curr(play), play(L), print('Did you play with your '), member(X, L), \+asked(X), assert(asked(X)), print(X), print(' today? y/n/q: '), read(Done), (Done==q -> abort;Done==y -> assert(done(X)), retract(curr(Y)), ask(0);assert(not_done(X)), ask_follow_up(X));
		curr(eat), eat(L), print('Did you eat '), member(X, L), \+asked(X), assert(asked(X)), print(X), print(' today? y/n/q: '), read(Done), (Done==q -> abort;Done==y -> assert(done(X)), retract(curr(Y)), ask(0);assert(not_done(X)), ask_follow_up(X)).


activity([play, eat, watch, learn]).

play([friends, giraffe, basketball, puzzle]).
eat([pizza, chicken, beef, burger, veggies]).

done(nothing).
not_done(nothing).