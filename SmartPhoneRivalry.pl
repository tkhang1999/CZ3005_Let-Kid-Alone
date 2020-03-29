company(sumSum).
company(appy).
competitor(sumSum, appy).
smartPhoneTech(galacticaS3).
develop(sumSum, galacticaS3).
boss(stevey).
steal(stevey, galacticaS3).
rival(X) :- competitor(X, appy).
business(X) :- smartPhoneTech(X).
unethical(X) :- 
    boss(X),rival(Y), company(Y), business(Z),
    develop(Y, Z), steal(X, Z).
