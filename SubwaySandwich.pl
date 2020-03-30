/*
 * Declare dynamic predicates to store selected items
 */
:- dynamic meal/1, bread/1, main/1, cheese/1, veg/1, sauce/1, side/1, drink/1.

/*
 * Interactive item selection for each category 
 */
select_meal :-
    read(X),
    selected(X, meals) -> nl, assertz(meal(X));
    write('Invalid meal, select again'), nl,
    select_meal.

select_bread :-
    read(X),
    selected(X, breads) -> nl, assertz(bread(X));
    write('Invalid bread, select again'), nl,
    select_bread.

select_main :-
    read(X),
    selected(X, mains) -> nl, assertz(main(X));
    write('Invalid main, select again'), nl,
    select_main. 

select_cheese :-
    read(X),
    (not(X == 0) ->
        (selected(X, cheeses) ->  write(' (Enter 0 to finish selecting cheeses)'), nl, assertz(cheese(X));
        write('Invalid cheese, select again'), nl),
        select_cheese;
        true
    ). 

select_veg :-
    read(X),
    (not(X == 0) -> 
        (selected(X, vegs) -> write(' (Enter 0 to finish selecting vegs)'), nl, assertz(veg(X));
        write('Invalid veg, select again'), nl),
        select_veg;
        true
    ).

select_sauce :-
    read(X),
    (not(X == 0) -> 
        (selected(X, sauces) -> write(' (Enter 0 to finish selecting sauces)'), nl, assertz(sauce(X));
        write('Invalid sauce, select again'), nl),
        select_sauce;
        true
    ).

select_healthy_sauce :-
    read(X),
    (not(X == 0) -> 
        (selected(X, healthy_sauces) -> write(' (Enter 0 to finish selecting healthy sauces)'), nl, assertz(sauce(X));
        write('Invalid healthy sauce, select again'), nl),
        select_healthy_sauce;
        true
    ).

select_side :-
    read(X),
    (not(X == 0) -> 
        (selected(X, sides) -> write(' (Enter 0 to finish selecting sides)'), nl, assertz(side(X));
        write('Invalid sides, select again'), nl),
        select_side;
        true
    ).

select_drink :-
    read(X),
    (not(X == 0) -> 
        (selected(X, drinks) -> write(' (Enter 0 to finish selecting drinks)'), nl, assertz(drink(X));
        write('Invalid drinks, try again'), nl),
        select_drink;
        true
    ).

/*
 * Interactive query for each category
 */
query_meal :-
    write('Please choose meal: '), nl,
    options(meals),
    select_meal.

query_bread :-
    write('Please choose bread: '), nl,
    options(breads),
    select_bread.

query_main :-
    write('Please choose main: '), nl,
    options(mains),
    select_main.

query_cheese :-
    write('Please choose cheese: '), nl,
    options(cheeses),
    select_cheese.

query_veg :-
    write('Please choose veg: '), nl,
    options(vegs),
    select_veg.

query_sauce :-
    write('Please choose sauce: '), nl,
    options(sauces),
    select_sauce.

query_healthy_sauce :-
    write('Please choose healthy sauce: '), nl,
    options(healthy_sauces),
    select_healthy_sauce.

query_side :-
    write('Please choose side: '), nl,
    options(sides),
    select_side.

query_drink :-
    write('Please choose drink: '), nl,
    options(drinks),
    select_drink.

/*
 * Query flow regarding meal type (e.g. normal, vegan, veggie, value)
 */ 
% all options are offered
meal_normal :-
    query_bread, query_main, query_cheese, query_veg,
    query_sauce, query_side, query_drink.

% meat option is not offered
meal_veggie :-
    query_bread, query_cheese, query_veg,
    query_healthy_sauce, query_side, query_drink.

% only healthy sauce option is offered (no fatty sauce)
meal_healthy :-
    query_bread, query_main, query_cheese, query_veg,
    query_healthy_sauce, query_side, query_drink.

% meat and cheese options are not offered
meal_vegan :-
    query_bread, query_veg,
    query_healthy_sauce, query_side, query_drink.

% side option is not offered (no top-up, i.e. cheese, side)
meal_value :-
    query_bread, query_main, query_veg, 
    query_sauce, query_drink.

/*
 * Get all selected options 
 */
get_selected(Meals, Breads, Mains, Cheeses, Vegs, Sauces, Sides, Drinks) :-
    findall(X, meal(X), Meals),
    findall(X, bread(X), Breads),
    findall(X, main(X), Mains),
    findall(X, cheese(X), Cheeses),
    findall(X, veg(X), Vegs),
    findall(X, sauce(X), Sauces),
    findall(X, side(X), Sides),
    findall(X, drink(X), Drinks).

/*
 * Display all selected options 
 */
display :-
    get_selected(Meals, Breads, Mains, Cheeses, Vegs, Sauces, Sides, Drinks), 
    atomic_list_concat(Meals, Meal),
    write('Meal: '), write(Meal), nl,
    atomic_list_concat(Breads, Bread),
    write('Bread: '), write(Bread), nl,
    atomic_list_concat(Mains, Main),
    write('Main: '), (not(Main == '') -> write(Main); write(none)), nl,
    atomic_list_concat(Cheeses, ',', Cheese),
    write('Cheeses: '), (not(Cheese == '') -> write(Cheese); write(none)), nl,
    atomic_list_concat(Vegs, ',', Veg),
    write('Vegs: '), (not(Veg == '') -> write(Veg); write(none)), nl,
    atomic_list_concat(Sauces, ',', Sauce),
    write('Sauces: '), (not(Sauce == '') -> write(Sauce); write(none)), nl,
    atomic_list_concat(Sides, ',', Side),
    write('Sides: '), (not(Side == '') -> write(Side); write(none)), nl,
    atomic_list_concat(Drinks, ',', Drink),
    write('Drinks: '), (not(Drink == '') -> write(Drink); write(none)), nl.

/*
 * Clean all selected options from memory
 */
clean :- retract(meal(_)), fail.
clean :- retract(bread(_)), fail.
clean :- retract(main(_)), fail.
clean :- retract(cheese(_)), fail.
clean :- retract(veg(_)), fail.
clean :- retract(sauce(_)), fail.
clean :- retract(side(_)), fail.
clean :- retract(drink(_)), fail.

/*
 * Start a Subway order
 */
start :-
    write('-----------------------------------------------'), nl,
    write('------------------START-ORDER------------------'), nl,
    write('-----------------------------------------------'), nl,
    exec,
    end.

/*
 * Execute a Subway order, normal by default
 */
exec :-
    query_meal, meal(Meal),
    ((Meal == veggie) -> 
        meal_veggie;
        (Meal == healthy) ->
            meal_healthy;
            (Meal == vegan) ->
                meal_vegan;
                (Meal == value) ->
                    meal_value;
                    meal_normal),
    write('------------------------------------------------'), nl,
    write('-------------------YOUR-ORDER-------------------'), nl,
    write('------------------------------------------------'), nl,
    display.

/*
 * End a Subway order
 */
end :-
    write('-----------------------------------------------'), nl,
    write('-------------------END-ORDER-------------------'), nl,
    write('-----------------------------------------------'),
    clean. % clean all selected options

/*
 * Display the list item
 */
options_list([]). % empty list
options_list([X]) :- write(X), write('.'), nl. % final list item
options_list([X|Y]) :- write(X), write(', '), options_list(Y), !. % sublist item

/*
 * Display the options for the list name
 */
options(meals):- meals(L), write('meals = '), options_list(L).
options(breads):- breads(L), write('breads = '), options_list(L).
options(mains):- mains(L), write('mains = '), options_list(L).
options(cheeses):- cheeses(L), write('cheeses = '), options_list(L).
options(vegs):- vegs(L), write('vegs = '), options_list(L).
options(sauces):- sauces(L), write('sauces = '), options_list(L).
options(healthy_sauces):- healthy_sauces(L), write('healthy sauces = '), options_list(L).
options(sides):- sides(L), write('sides = '), options_list(L).
options(drinks):- drinks(L), write('drinks = '), options_list(L).

/*
 * Select an option
 */
selected(X, meals) :- meals(L), member(X, L), write('meal = '), write(X), !.
selected(X, breads) :- breads(L), member(X, L), write('bread = '), write(X), !.
selected(X, mains) :- mains(L), member(X, L), write('main = '), write(X), !.
selected(X, cheeses) :- cheeses(L), member(X, L), write('cheese = '), write(X), !.
selected(X, vegs) :- vegs(L), member(X, L), write('veg = '), write(X), !.
selected(X, sauces) :- sauces(L), member(X, L), write('sauce = '), write(X), !.
selected(X, healthy_sauces) :- healthy_sauces(L), member(X, L), write('healthy sauce = '), write(X), !.
selected(X, sides) :- sides(L), member(X, L), write('side = '), write(X), !.
selected(X, drinks) :- drinks(L), member(X, L), write('drink = '), write(X), !.

/*
 * Possible choice of options for each category
 */
meals([normal, veggie, healthy, vegan, value]).
breads([italian_wheat, hearty_italian, honey_oat, parmesan_oregano, multigrain, flat_bread]).
mains([ham, chicken, tuna, turkey, roast_beef, meatball, egg_mayo, italian_bmt, steak_and_cheese]).
cheeses([american, monterey, feta, mozzarella, cheddar, pepperjack, provolone, swiss, none]).
vegs([cucumbers, green_bell_peppers, lettuce, red_onions, tomatoes, black_olives, jalapenos, pickles, none]).
sauces([chipotle_southwest, ranch, bbq, chilli_sauce, tomato_sauce, redwine, mayonnaise, vinegar, none]).
healthy_sauces([chipotle_southwest, ranch, tomato_sauce, redwine, vinegar, none]).
sides([chips, cookies, hashbrowns, fruit_crisps, yogurt, none]).
drinks([fountain, mineral_water, orange_juice, green_tea, coffee, tea, none]).
