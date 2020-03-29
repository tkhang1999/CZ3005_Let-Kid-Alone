:- use_module(library(jpl)).


male(charles).
male(andrew).
male(edward).
female(ann).

older(charles, ann).
older(ann, andrew).
older(andrew, edward).

elder(X, Y) :- older(X, Y).
elder(X, Y) :- older(X, Z), older(Z, Y).

priority(X, Y) :- male(X), female(Y).
priority(X, Y) :- male(X), male(Y), elder(X, Y).
priority(X, Y) :- female(X), female(Y), elder(X, Y).

successionLine([X, Y, Z, T]) :- 
    priority(X, Y), priority(Y, Z), priority(Z, T).
