% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

%?- consult(prologpather.pl).
:- ['prologpather.pl'].

:- begin_tests('member').

% destination correct input case 1
test('Nest all correct') :-
  destination("Nest", Result, Time),
  assertion(Result == "To get to the Nest: Go NORTH. Transit time: 2 minutes."),
  assertion(Time == 2).

% destination correct input case 2
test('IKB all correct') :-
  destination("IKB", Result, Time),
  assertion(Result == "To get to IKB: Go NORTH, then turn RIGHT on E mall. Transit time: 4 minutes."),
  assertion(Time == 4).

% destination correct input case 3
test('Bookstore all correct ') :-
  destination("Bookstore", Result, Time),
  assertion(Result == "To get to the Bookstore: Go NORTH, then turn LEFT on E mall. Transit time: 3 minutes."),
  assertion(Time == 3).

% destination correct input case 4
test('Nest NOT longer route') :-
  destination("Nest", Result, Time),
  assertion(Result \= "This is a horrible set of directions! Go NORTH. Then SOUTH. Then NORTH. Then SOUTH. Then NORTH. Transit time: 10 minutes."),
  assertion(Time \= 10).

:- end_tests('member').
