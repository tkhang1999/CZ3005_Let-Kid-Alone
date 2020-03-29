male(charles).
male(andrew).
male(edward).
female(ann).

older(charles, ann).
older(ann, andrew).
older(andrew, edward).

elder(X, Y) :- older(X, Y).
elder(X, Y) :- older(X, Z), older(Z, Y).

successionLine([X, Y, Z, T]) :- 
    elder(X, Y), elder(Y, Z), elder(Z, T).
