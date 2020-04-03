/*
 * Declare dynamic predicates to store 
 * yes/no answers for activity query, and already asked questions
 */
:- dynamic yes/1, no/1, asked/1.


/*
 * List of all available activities
 */
activity([play, eat, do, see, learn, behave]).

/* 
 * List of all related questions for each activity
 */
play(["Did you play toys", "Did you play cars", "Did you play teddy bears",
    "Did you play with friends", "Did you play sandbox", "Did you play trains"]).
eat(["Did you eat candy", "Did you eat cheerios", "Did you eat sandwich",
    "Did you eat toffee", "Did you eat veggies", "Did you eat fries"]).
do(["Did you build anything", "Did you make any origami", "Did you wash hand",
    "Did you use fork or spoon", "Did you draw anything", "Did you exercise"]).
see(["Did you see pictures", "Did you see blocks", "Did you see alphabets",
    "Did you see playmat", "Did you see cars", "Did you see cake"]).
learn(["Did you learn math", "Did you learn English", "Did you learn Chinese",
    "Did you learn art", "Did you learn singing", "Did you learn drawing"]).
behave(["Did you say 'please' when asking", "Did you help clean up the class",
    "Did you say 'hi' to teachers", "Did you say 'thank you' to teachers", 
    "Did you say 'goodbye' to friends", "Did you share snacks with friends"]).


/*
 * Check the activity list, if it is empty, then end asking questions
 */
query_activity([]) :- 
    nl, write("Wish you a better day tomorrow!"),
    nl, end.

/*
 * Check the activity list, if it is not empty,
 * then query the kid about this activity
 * 
 * Kid must reply with a valid answer (yes/no/quit);
 * otherwise, kid must answer again
 */
query_activity(L) :-
    member(X, L), nl, write("Did you "), write(X),
    write(" well at school"), write("? (yes/no/quit)"), nl,
    read(Answer),
    ((Answer == yes) ->
        answer_yes(X);
        (Answer == no) ->
            answer_no(X);
            (Answer == quit) ->
                end;
                write("---Invalid answer, please answer again!---"),
                nl, query_activity(L)).


/*
 * Handle yes and no answers for each activity individually, 
 * 
 * if the answer is yes, save it to the "yes" predicate,
 * and start asking follow up questions in that activity
 * 
 * else, save the no answer to the "no" predicate,
 * and move on to query the kid about unasked activities
 */
answer_yes(X) :- 
    assertz(yes(X)), first_follow_up(X, L), query_unasked_follow_up(L).
answer_no(X) :- 
    assertz(no(X)), query_unasked_activity.

/*
 * Query the unasked activity list to the kid
 */
query_unasked_activity :- unasked_activity_list(L), query_activity(L).

/*
 * Get a list of unasked activities
 */
unasked_activity_list(L) :- findnsols(6, X, unasked_activity(X), L).

/*
 * Return random unasked activity
 */
unasked_activity(X) :- 
    activity(A), findnsols(6, Y, yes(Y), Yes), findnsols(6, N, no(N), No), 
    append(Yes, No, Asked), list_to_set(A, S), list_to_set(Asked, H), 
    subtract(S, H, Unasked), random_member(X, Unasked).


/*
 * Get a list of follow up questions related to the activity X
 */
first_follow_up(X, L) :- findnsols(6, Y, related(X, Y), L).

/*
 * Get a list of unasked follow up questions
 * and proceed to check these unasked questions
 */
query_unasked_follow_up(L) :-
    findnsols(6, X, asked(X), Asked), list_to_set(L, S), list_to_set(Asked, A),
    subtract(S, A, Unasked), check_unasked_follow_up(Unasked).


/*
 * Check the list of unasked follow up questions list,
 * if the list is empty, proceed to query the kid about
 * other unasked activities
 */
check_unasked_follow_up([]) :- query_unasked_activity.

/*
 * Check the list of unasked follow up questions list,
 * if the list is not empty, query the kid with a question
 * and then proceed to ask another one
 * 
 * Kid must reply with a valid answer (yes/no/quit);
 * otherwise, kid must answer again
 */
check_unasked_follow_up(X) :-
    member(Y, X), write(Y), write("? (yes/no/quit): "),
    read(Answer),
    ((Answer == yes) -> 
        assertz(asked(Y));
        (Answer == no) ->
            assertz(asked(Y));
            (Answer == quit) ->
                end;
                write("---Invalid answer, please answer again!---"),
                nl, check_unasked_follow_up(X)),
    next_follow_up(Y).

/*
 * Get a list of all options for next follow up questions,
 * then proceed to query next unasked follow up questions
 */
next_follow_up(X) :- options_follow_up(X, L), query_unasked_follow_up(L).

/*
 * Get all options for follow up questions related with question X
 */
options_follow_up(X, L) :- findnsols(6, Y, related_follow_up(X, Y), L).

/*
 * Check if question Y is related to the same activity with question X
 * and question Y is an unasked question
 */
related_follow_up(X, Y) :- eat(L), member(X, L), member(Y, L), \+asked(Y).
related_follow_up(X, Y) :- play(L), member(X, L), member(Y, L), \+asked(Y).
related_follow_up(X, Y) :- do(L), member(X, L), member(Y, L), \+asked(Y).
related_follow_up(X, Y) :- see(L), member(X, L), member(Y, L), \+asked(Y).
related_follow_up(X, Y) :- learn(L), member(X, L), member(Y, L), \+asked(Y).
related_follow_up(X, Y) :- behave(L), member(X, L), member(Y, L), \+asked(Y).


/*
 * Get a random question form a topic
 */
related(play, X):- play(L), random_member(X, L).
related(eat, X):- eat(L), random_member(X, L).
related(do, X):- do(L), random_member(X, L).
related(see, X):- see(L), random_member(X, L).
related(learn, X):- learn(L), random_member(X, L).
related(behave, X) :- behave(L), random_member(X, L).


/*
 * Start to ask the kid about his/her day
 */
start :-
    write("-----------------------------------------------"), nl,
    write("---------------------START---------------------"), nl,
    write("-----------------------------------------------"), nl,
    write("How was your day my kid?"), nl,
    query_unasked_activity.

/*
 * End asking the kid questions
 */
end :-
    write("-----------------------------------------------"), nl,
    write("----------------------END----------------------"), nl,
    write("-----------------------------------------------"), nl,
    clean, abort.

/*
 * Clean all saved data from memory
 */
clean :- 
    retractall(yes(_)),
    retractall(no(_)),
    retractall(asked(_)).
