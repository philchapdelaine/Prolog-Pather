%OUR PROJECT WAS WORKING BUT THE VERSION CHANGE CAUSED ERRORS:
%Before the SWIPL rollback, calling "start." and then selecting a destination "Nest.","IKB.", or "Bookstore." would return the destination with the shortest time taken on the first call,
%regardless of the order they were defined. However, after the rollback, it now selects the first one defined, regardless of time taken.
:- use_module(library(clpfd)).

start :-
    writeln("Welcome to PrologPather! Please select a destination:"), findall(X, destination(X,_,_), L), print_destinations(L),
    goto(Input).
%Find all valid destinations referred to with destination/2, and print them

%if Input matches a destination's key string, output it's directions, otherwise ask for a valid input
goto(Input) :- read_line_to_string(current_input, Input),destination(Input,Directions,T1),destination(Input,Directions2,T2),print_shorter(T1,T2,Directions,Directions2).
goto(Input) :- destination(Input,Directions,_),destination(Input,Directions2,_),Directions=Directions2,writeln(Directions).
goto(Input) :- writeln("Please input a valid destination"),goto(Input).

%Using hardcoded version to reduce debugging headaches for proof of concept - Dynamic printing will return for final project
print_destinations(_) :- writeln("Nest"),writeln("IKB"),writeln("Bookstore").

print_shorter(T1,T2,D1,D2) :- T1#<T2,writeln(D1).
print_shorter(T1,T2,D1,D2) :- T2#>T1,writeln(D2).

% Destination database
destination("Nest","To get to the Nest: Go NORTH. Transit time: 2 minutes.",2).
destination("Nest","This is a horrible set of directions! Go NORTH. Then SOUTH. Then NORTH. Then SOUTH. Then NORTH. Transit time: 10 minutes.",10).
destination("IKB", "To get to IKB: Go NORTH, then turn RIGHT on E mall. Transit time: 4 minutes.",4).
destination("IKB","To get to IKB slowly: Go NORTH, then wait for 10 minutes. Then turn right on E mall. Transit time: 14 minutes.",14).
destination("Bookstore", "To get to the Bookstore: Go NORTH, then turn LEFT on E mall. Transit time: 3 minutes.",3).
destination("Bookstore","Bookstore longcut: Go NORTH, then go SOUTH, then North, then turn LEFT on E mall. Transit time: 5 minutes.",5).





%recurisvely print a list of strings (in this case, destinations)
% print_destinations([FirstDest|RestDest]) :- writeln(FirstDest),print_destinations(RestDest).
% print_destinations([]) :- writeln("").
