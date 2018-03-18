:- module(io, [set_prompt/0, read_input/1, print_output/1]).


% Set the user prompt
set_prompt :- prompt(_, 'λ-προ: ').

% Read a line from the user's input
read_input(In) :- read_string(user_input, "\n", "\t", _, In).

% Otherwise it'd be rewl
print_output(Out) :- writeln(Out).
