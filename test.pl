:- module(test, [test/0]).

:- use_module(lib/utils).
:- use_module(lib/terms).
:- use_module(lib/reduction).
:- use_module(lib/evaluate).
:- load_test_files([]).


test :- run_tests.
% :- show_coverage(run_tests).
